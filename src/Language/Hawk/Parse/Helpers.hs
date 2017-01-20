{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Language.Hawk.Parse.Helpers where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Text.PrettyPrint.ANSI.Leijen (pretty, Pretty, putDoc)
import Text.Earley
import Text.Earley.Mixfix
import Data.Text.Lazy (Text)

import qualified Data.ByteString.UTF8         as UTF8
import qualified Data.Text.Lazy               as Text
import qualified Language.Hawk.Parse.Lexer    as L
import qualified Language.Hawk.Report.Region  as R
import qualified Language.Hawk.Syntax.Name    as Name
import qualified Language.Hawk.Syntax.Type    as Ty

-- -----------------------------------------------------------------------------
-- Parser type
--type HkProd a = forall r. Prod r L.Token L.Token a
--type HkGrammar a = forall r. Grammar r (Prod r L.Token L.Token a)

parseTest :: T 
parseTest i =
  print $ fullParses (parser expr) i


-- -----------------------------------------------------------------------------
-- Helpers for parsing expressions

type OpTable expr ident = [[(Holey ident, Associativity, (Holey ident -> [expr] -> expr))]]

holey :: String -> Holey (Prod r L.Token L.Token L.Token)
holey ""       = []
holey ('_':xs) = Nothing : holey xs
holey xs       = Just (sym i) : holey rest
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
  where p (L.Token _ c') = c == c'
  
sym :: String -> Prod r e L.Token L.Token
sym str = satisfy p
  where p (L.Token _ (L.TokenSymbol str')) = str == str' 
        p _ = False
        
sym' :: String -> Prod r e L.Token String
sym' str = sym str *> pure str
        
parens :: Prod r e L.Token a -> Prod r e L.Token a
parens p =
  sym "(" *> p <* sym ")"

sep :: String -> Prod r e L.Token a -> Prod r e L.Token [a]
sep s p =
  (:) <$> p <*> many (sym s *> p)
  
sep' :: String -> Prod r e L.Token a -> Prod r e L.Token [a]
sep' s p =
  sep s p <|> pure []

-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Name Tokens

varId :: Prod r e L.Token Name.Source
varId = fmap unsafeExtract (satisfy p)
  where
    p (L.Token _ (L.TokenVarId _)) = True
    p  _                             = False
    unsafeExtract (L.Token i (L.TokenVarId v)) = Name.Name (Name.Local . R.toPosition $ L.tokiPos i) v

conId :: Prod r e L.Token Name.Source
conId = fmap unsafeExtract (satisfy p)
  where
    p (L.Token _ (L.TokenConId _)) = True
    p  _                           = False
    unsafeExtract (L.Token i (L.TokenConId v)) = Name.Name (Name.Local . R.toPosition $ L.tokiPos i) v


modId :: Prod r e L.Token String
modId = fmap unsafeExtract (satisfy p)
  where
    p (L.Token _ (L.TokenConId _)) = True
    p  _                           = False
    unsafeExtract (L.Token _ (L.TokenConId v)) = v

-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Literal Tokens
tInteger :: Prod r e L.Token Integer
tInteger = fmap unsafeExtract (satisfy p)
  where
    p (L.Token _ (L.TokenInteger _)) = True
    p  _                             = False
    unsafeExtract (L.Token _ (L.TokenInteger v)) = v

tReal :: Prod r e L.Token Double
tReal = fmap unsafeExtract (satisfy p)
  where
    p (L.Token _ (L.TokenDouble _)) = True
    p  _                             = False
    unsafeExtract (L.Token _ (L.TokenDouble v)) = v

tChar :: Prod r e L.Token Char
tChar = fmap unsafeExtract (satisfy p)
  where
    p (L.Token _ (L.TokenChar _)) = True
    p  _                             = False
    unsafeExtract (L.Token _ (L.TokenChar v)) = v

tString :: Prod r e L.Token String
tString = fmap unsafeExtract (satisfy p)
  where
    p (L.Token _ (L.TokenString _)) = True
    p  _                             = False
    unsafeExtract (L.Token _ (L.TokenString v)) = v

tBool :: Prod r e L.Token Bool
tBool = fmap unsafeExtract (satisfy p)
  where
    p (L.Token _ (L.TokenBool _)) = True
    p  _                             = False
    unsafeExtract (L.Token _ (L.TokenBool v)) = v


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
  