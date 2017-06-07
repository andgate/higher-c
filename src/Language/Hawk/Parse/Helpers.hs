{-# LANGUAGE ExistentialQuantification
           , RankNTypes
           , OverloadedStrings
  #-}
module Language.Hawk.Parse.Helpers where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (Except, throwE, runExceptT)
import Control.Monad.Trans.State.Strict (evalState)
import Data.Text.Buildable (Buildable(..))
import Data.Text (Text)
import Data.Word (Word8)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax
import Text.PrettyPrint.ANSI.Leijen (pretty, Pretty, putDoc)
import Text.Earley
import Text.Earley.Mixfix

import qualified Data.ByteString.UTF8             as UTF8
import qualified Data.Text                        as Text
import qualified Language.Hawk.Report.Region      as R

-- -----------------------------------------------------------------------------
-- Parser type
--type HkProd a = forall r. Prod r Token Token a
--type HkGrammar a = forall r. Grammar r (Prod r Token Token a)
        
--type OpTable a = forall r. [[(Holey (Prod r Token Token Token), Associativity, Holey Token -> [a] -> a)]]
-- type TypeOpTable = OpTable Ty.Typed
-- type ExprOpTable = OpTable E.Source

-- -----------------------------------------------------------------------------
-- Mixfix

{-

holey :: String -> Holey (Prod r Token Token Token)
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

  
typArrow :: Holey Token -> [Ty.Typed] -> Ty.Typed
typArrow _ = Ty.typeCon "_->_"

typParens :: Holey Token -> [Ty.Typed] -> Ty.Typed
typParens _ = Ty.typeCon "(_)"

typDollar :: Holey Token  -> [Ty.Typed] -> Ty.Typed
typDollar _ = Ty.typeCon "_$_"

-}

-- -----------------------------------------------------------------------------
-- Terminal Production Helpers
match :: TokenClass -> Prod r e Token Token
match c = satisfy p
  where p (Token c' _ _ _) = c == c'

rsvp :: Text -> Prod r e Token Token
rsvp text =
  match $ TokenRsvp text

prim :: Text -> Prod r e Token Token
prim text =
  match $ TokenPrim text

-- -----------------------------------------------------------------------------
-- Combinator Helpers

parens :: Prod r e Token a -> Prod r e Token a
parens p =
  rsvp "(" *> p <* rsvp ")"

sqrBrackets :: Prod r e Token a -> Prod r e Token a
sqrBrackets p =
  rsvp "[" *> p <* rsvp "]"

curlyBrackets :: Prod r e Token a -> Prod r e Token a
curlyBrackets p =
  rsvp "{" *> p <* rsvp "}"

angleBrackets :: Prod r e Token a -> Prod r e Token a
angleBrackets p =
  rsvp "<" *> p <* rsvp ">"

           
sep :: Prod r e Token b -> Prod r e Token a -> Prod r e Token [a]
sep s p =
  (:) <$> p <*> many (s *> p)
  
sep' :: Prod r e Token b -> Prod r e Token a -> Prod r e Token [a]
sep' s p =
  sep s p <|> pure []
  
mono :: Prod r e Token a -> Prod r e Token [a]
mono =
  fmap (:[])
  
prepend :: Prod r e Token a -> Prod r e Token [a] -> Prod r e Token [a]  
prepend =
  liftA2 (:)

-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Name Tokens

varName :: Prod r e Token Name
varName = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenVarId _) _ _ _) = True
    p  _                         = False
    unsafeExtract (Token (TokenVarId n) _ fp r) = Name n (Home fp r)

conName :: Prod r e Token Name
conName = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenConId _) _ _ _) = True
    p  _                         = False
    unsafeExtract (Token (TokenVarId n) _ fp r) = Name n (Home fp r)

opName :: Prod r e Token Name
opName = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenOpId _) _ _ _) = True
    p  _                        = False
    unsafeExtract (Token (TokenOpId n) _ fp r) = Name n (Home fp r)


-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Literal Tokens
tInteger :: Prod r e Token Integer
tInteger = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenInteger _) _ _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenInteger v) _ _ _) = v

tReal :: Prod r e Token Double
tReal = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenDouble _) _ _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenDouble v) _ _ _) = v

tChar :: Prod r e Token Char
tChar = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenChar _) _ _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenChar v) _ _ _) = v

tString :: Prod r e Token String
tString = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenString _) _ _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenString v) _ _ _) = v

tBool :: Prod r e Token Bool
tBool = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenBool _) _ _ _) = True
    p  _                             = False
    unsafeExtract (Token (TokenBool v) _ _ _) = v


-- -----------------------------------------------------------------------------
-- Layout Helpers

eof :: Prod r e Token Token
eof = match TokenEof

block :: Prod r e Token a -> Prod r e Token [a]
block p = blk *> linefolds p <* blk'

linefolds0 :: Prod r e Token a -> Prod r e Token [a]
linefolds0 p = many $ linefold p

linefolds :: Prod r e Token a -> Prod r e Token [a]
linefolds p = some $ linefold p

linefold :: Prod r e Token a -> Prod r e Token a
linefold p = ln *> p <* ln'


blk :: Prod r e Token Token
blk = match TokenBlk

blk' :: Prod r e Token Token
blk' = match TokenBlk'


ln :: Prod r e Token Token
ln = match TokenLn

ln' :: Prod r e Token Token
ln' = match TokenLn'