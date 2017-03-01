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

import qualified Data.ByteString.UTF8             as UTF8
import qualified Data.Text.Lazy                   as Text
import qualified Language.Hawk.Parse.Lexer        as L
import qualified Language.Hawk.Report.Region      as R
import qualified Language.Hawk.Syntax.Expression  as E
import qualified Language.Hawk.Syntax.Module      as M
import qualified Language.Hawk.Syntax.Name        as N
import qualified Language.Hawk.Syntax.Type        as Ty
import qualified Pipes.Prelude                    as Pipes

-- -----------------------------------------------------------------------------
-- Parser type
--type HkProd a = forall r. Prod r L.Token L.Token a
--type HkGrammar a = forall r. Grammar r (Prod r L.Token L.Token a)
        
type OpTable a = forall r. [[(Holey (Prod r L.Token L.Token L.Token), Associativity, Holey L.Token -> [a] -> a)]]
type TypeOpTable = OpTable Ty.Typed
type ExprOpTable = OpTable E.Source

-- -----------------------------------------------------------------------------
-- Helpers for parsing expressions

holey :: String -> Holey (Prod r L.Token L.Token L.Token)
holey ""       = []
holey ('_':xs) = Nothing : holey xs
holey xs       = Just (op $ Text.pack i) : holey rest
  where (i, rest) = span (/= '_') xs

defExprOps :: ExprOpTable
defExprOps =
  []
  
defTypeOps :: TypeOpTable
defTypeOps =
  [ [(holey "_->_", RightAssoc, typArrow)]
  , [(holey "_$_", RightAssoc, typDollar)]
  ]

  
typArrow :: Holey L.Token -> [Ty.Typed] -> Ty.Typed
typArrow _ = Ty.typeCon "_->_"

typParens :: Holey L.Token -> [Ty.Typed] -> Ty.Typed
typParens _ = Ty.typeCon "(_)"

typDollar :: Holey L.Token  -> [Ty.Typed] -> Ty.Typed
typDollar _ = Ty.typeCon "_$_"


-- -----------------------------------------------------------------------------
-- Terminal Production Helpers
match :: L.TokenClass -> Prod r e L.Token L.Token
match c = satisfy p
  where p (L.Token c' _) = c == c'
  
notToken :: L.TokenClass -> Prod r e L.Token L.Token
notToken c = satisfy p
  where p (L.Token c' _) = c /= c'
  
notTokens :: [L.TokenClass] -> Prod r e L.Token L.Token
notTokens cs  = satisfy p
  where p (L.Token c' _) = c' `notElem` cs


notEquals :: Prod r e L.Token L.Token
notEquals = notToken $ L.TokenRsvp "="

tillEquals  :: Prod r e L.Token [L.Token]
tillEquals = many notEquals

notTypeCtxArr :: Prod r e L.Token L.Token
notTypeCtxArr = notToken $ L.TokenRsvp "=>"

notColon :: Prod r e L.Token L.Token
notColon = notToken $ L.TokenRsvp ":"

notLayout :: Prod r e L.Token L.Token
notLayout =
  notTokens [ L.TokenTop
            , L.TokenBlk
            , L.TokenBlk'
            , L.TokenLn
            , L.TokenLn'
            , L.TokenRsvp "("
            , L.TokenRsvp ")"
            ]
  
  
anyToken :: Prod r e L.Token L.Token
anyToken = satisfy p
  where p _ = True
  
rsvp :: Text -> Prod r e L.Token L.Token
rsvp text =
  match $ L.TokenRsvp text
           
 
parens :: Prod r e L.Token a -> Prod r e L.Token a
parens p =
  rsvp "(" *> p <* rsvp ")"

           
sep :: Prod r e L.Token b -> Prod r e L.Token a -> Prod r e L.Token [a]
sep s p =
  (:) <$> p <*> many (s *> p)
  
sep' :: Prod r e L.Token b -> Prod r e L.Token a -> Prod r e L.Token [a]
sep' s p =
  sep s p <|> pure []

-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Name Tokens

varTok :: Prod r e L.Token L.Token
varTok = satisfy p
  where
    p (L.Token (L.TokenVarId _) _) = True
    p  _                             = False

varId :: Prod r e L.Token N.Source
varId = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenVarId _) _) = True
    p  _                             = False
    unsafeExtract (L.Token (L.TokenVarId v) p) = N.Name v p

conTok :: Prod r e L.Token L.Token
conTok = satisfy p
  where
    p (L.Token (L.TokenConId _) _) = True
    p  _                             = False

conId :: Prod r e L.Token N.Source
conId = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenConId _) _) = True
    p  _                           = False
    unsafeExtract (L.Token (L.TokenConId v) p) = N.Name v p
    

opId :: Prod r e L.Token N.Source
opId = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenOpId _) _) = True
    p _                           = False
    unsafeExtract (L.Token (L.TokenOpId v) p) = N.Name v p


opNamed :: Text -> Prod r e L.Token N.Source
opNamed t = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenOpId t') _) = t == t'
    p _                           = False
    unsafeExtract (L.Token (L.TokenOpId v) p) = N.Name v p


named :: Text -> Prod r e L.Token N.Source
named txt = fmap unsafeExtract (satisfy p)
  where
    p (L.Token (L.TokenRsvp _) _)  = True
    p (L.Token (L.TokenVarId _) _) = True
    p (L.Token (L.TokenConId _) _) = True
    p (L.Token (L.TokenOpId _) _)  = True
    p  _                           = False
    
    unsafeExtract (L.Token (L.TokenRsvp v) p) = N.Name v p
    unsafeExtract (L.Token (L.TokenVarId v) p) = N.Name v p
    unsafeExtract (L.Token (L.TokenConId v) p) = N.Name v p
    unsafeExtract (L.Token (L.TokenOpId v) p) = N.Name v p
    unsafeExtract _ = undefined


op :: Text -> Prod r e L.Token L.Token
op str = satisfy p
  where p (L.Token (L.TokenOpId str') _) = str == str' 
        p _ = False

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
-- Layout Helpers

eof :: Prod r e L.Token L.Token
eof = match L.TokenEof

block :: Prod r e L.Token a -> Prod r e L.Token [a]
block p = blk *> linefolds p <* blk'

linefolds0 :: Prod r e L.Token a -> Prod r e L.Token [a]
linefolds0 p = many $ linefold p

linefolds :: Prod r e L.Token a -> Prod r e L.Token [a]
linefolds p = some $ linefold p

linefold :: Prod r e L.Token a -> Prod r e L.Token a
linefold p = ln *> p <* ln'


blk :: Prod r e L.Token L.Token
blk = match L.TokenBlk

blk' :: Prod r e L.Token L.Token
blk' = match L.TokenBlk'


ln :: Prod r e L.Token L.Token
ln = match L.TokenLn

ln' :: Prod r e L.Token L.Token
ln' = match L.TokenLn'


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
  