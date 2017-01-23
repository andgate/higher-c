{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Language.Hawk.Parse.Helpers where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (Except, throwE, runExceptT)
import Control.Monad.Trans.State.Strict (evalState)
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Word (Word8)
import Text.PrettyPrint.ANSI.Leijen (pretty, Pretty, putDoc)
import Text.Earley
import Text.Earley.Mixfix

import qualified Data.ByteString.UTF8         as UTF8
import qualified Data.Text.Lazy               as Text
import qualified Language.Hawk.Parse.Lexer    as L
import qualified Language.Hawk.Report.Region  as R
import qualified Language.Hawk.Syntax.Module  as M
import qualified Language.Hawk.Syntax.Name    as Name
import qualified Language.Hawk.Syntax.Type    as Ty
import qualified Pipes.Prelude                as Pipes

-- -----------------------------------------------------------------------------
-- Parser type
--type HkProd a = forall r. Prod r L.Token L.Token a
--type HkGrammar a = forall r. Grammar r (Prod r L.Token L.Token a)
        


-- -----------------------------------------------------------------------------
-- Helpers for parsing expressions

type OpTable expr ident = [[(Holey ident, Associativity, (Holey ident -> [expr] -> expr))]]

holey :: String -> Holey (Prod r L.Token L.Token L.Token)
holey ""       = []
holey ('_':xs) = Nothing : holey xs
holey xs       = Just (op $ Text.pack i) : holey rest
  where (i, rest) = span (/= '_') xs

exprOpsTable :: [[(Holey (Prod r L.Token L.Token L.Token), Associativity)]]
exprOpsTable =
  [ [(holey "if_then_else_", RightAssoc)]
  , [(holey "_+_", LeftAssoc), (holey "_-_", LeftAssoc)]
  , [(holey "_*_", LeftAssoc), (holey "_/_", LeftAssoc)]
  , [(holey "(_)", NonAssoc), (holey"_$_", RightAssoc)]
  ]
  
typeOpsTable :: [[(Holey (Prod r L.Token L.Token L.Token), Associativity, Holey L.Token -> [Ty.Source] -> Ty.Source)]]
typeOpsTable =
  [ [(holey "->", RightAssoc, typArrow)]
  , [(holey "(_)", NonAssoc, typParens), (holey "_$_", RightAssoc, typDollar)]
  ]

  
typArrow :: Holey L.Token -> [Ty.Source] -> Ty.Source
typArrow _ = Ty.typeCon "_->_"

typParens :: Holey L.Token -> [Ty.Source] -> Ty.Source
typParens _ = Ty.typeCon "(_)"

typDollar :: Holey L.Token  -> [Ty.Source] -> Ty.Source
typDollar _ = Ty.typeCon "_$_"
  

-- -----------------------------------------------------------------------------
-- Terminal Production Helpers
match :: L.TokenClass -> Prod r e L.Token L.Token
match c = satisfy p
  where p (L.Token c' _) = c == c'
  
rsvp :: Text -> Prod r e L.Token L.Token
rsvp text =
  match $ L.TokenRsvp text
        
parens :: Prod r e L.Token a -> Prod r e L.Token a
parens p =
  op "(" *> p <* op ")"

sep :: Prod r e L.Token b -> Prod r e L.Token a -> Prod r e L.Token [a]
sep s p =
  (:) <$> p <*> many (s *> p)
  
sep' :: Prod r e L.Token b -> Prod r e L.Token a -> Prod r e L.Token [a]
sep' s p =
  sep s p <|> pure []

-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Name Tokens

varId :: Prod r e L.Token Name.Source
varId = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenVarId _) _) = True
    p  _                             = False
    unsafeExtract (L.Token (L.TokenVarId v) p) = Name.Name (Name.Local $ R.toPosition p) v

conId :: Prod r e L.Token Name.Source
conId = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenConId _) _) = True
    p  _                           = False
    unsafeExtract (L.Token (L.TokenConId v) p) = Name.Name (Name.Local $ R.toPosition p) v


modId :: Prod r e L.Token Text
modId = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenConId _) _) = True
    p  _                           = False
    unsafeExtract (L.Token (L.TokenConId v) _) = v
    

op :: Text -> Prod r e L.Token L.Token
op str = satisfy p
  where p (L.Token (L.TokenOpId str') _) = str == str' 
        p _ = False
        
op' :: Text -> Prod r e L.Token Text
op' str = op str *> pure str

-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Literal Tokens
tInteger :: Prod r e L.Token Integer
tInteger = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenInteger _) _) = True
    p  _                             = False
    unsafeExtract (L.Token (L.TokenInteger v) _) = v

tReal :: Prod r e L.Token Double
tReal = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenDouble _) _) = True
    p  _                             = False
    unsafeExtract (L.Token (L.TokenDouble v) _) = v

tChar :: Prod r e L.Token Char
tChar = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenChar _) _) = True
    p  _                             = False
    unsafeExtract (L.Token (L.TokenChar v) _) = v

tString :: Prod r e L.Token String
tString = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenString _) _) = True
    p  _                             = False
    unsafeExtract (L.Token (L.TokenString v) _) = v

tBool :: Prod r e L.Token Bool
tBool = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenBool _) _) = True
    p  _                             = False
    unsafeExtract (L.Token (L.TokenBool v) _) = v


-- -----------------------------------------------------------------------------
-- Location

-- Old location algorithms from the old parsec-style parser
{-
locate :: HkParser a -> HkParser (A.Located a)
locate p = do
  p1 <- getPosition
  r <- p
  p2 <- getPosition
  return $ A.A (R.mkRegion p1 p2) r
  
  
withRegion :: HkParser a -> (R.Region -> a -> b) -> HkParser b
withRegion p func = do
  p1 <- getPosition
  result <- p
  region <- R.mkRegion p1 <$> getPosition
  return $ func region result
  -}
  