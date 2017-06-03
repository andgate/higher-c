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
import Language.Hawk.Syntax.Source (Name)
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
        
type OpTable a = forall r. [[(Holey (Prod r Token Token Token), Associativity, Holey Token -> [a] -> a)]]
-- type TypeOpTable = OpTable Ty.Typed
-- type ExprOpTable = OpTable E.Source

-- -----------------------------------------------------------------------------
-- Helpers for parsing expressions

holey :: String -> Holey (Prod r Token Token Token)
holey ""       = []
holey ('_':xs) = Nothing : holey xs
holey xs       = Just (op $ Text.pack i) : holey rest
  where (i, rest) = span (/= '_') xs

{-

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
  where p (Token c' _) = c == c'
  
notToken :: TokenClass -> Prod r e Token Token
notToken c = satisfy p
  where p (Token c' _) = c /= c'
  
notTokens :: [TokenClass] -> Prod r e Token Token
notTokens cs  = satisfy p
  where p (Token c' _) = c' `notElem` cs


notEquals :: Prod r e Token Token
notEquals = notToken $ TokenRsvp "="

tillEquals  :: Prod r e Token [Token]
tillEquals = many notEquals

notTypeCtxArr :: Prod r e Token Token
notTypeCtxArr = notToken $ TokenRsvp "=>"

notColon :: Prod r e Token Token
notColon = notToken $ TokenRsvp ":"

notLayout :: Prod r e Token Token
notLayout =
  notTokens [ TokenTop
            , TokenBlk
            , TokenBlk'
            , TokenLn
            , TokenLn'
            , TokenRsvp "("
            , TokenRsvp ")"
            ]
  
  
anyToken :: Prod r e Token Token
anyToken = satisfy p
  where p _ = True
  
rsvp :: Text -> Prod r e Token Token
rsvp text =
  match $ TokenRsvp text
           
 
parens :: Prod r e Token a -> Prod r e Token a
parens p =
  rsvp "(" *> p <* rsvp ")"

brackets :: Prod r e Token a -> Prod r e Token a
brackets p =
  rsvp "[" *> p <* rsvp "]"

           
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

varId :: Prod r e Token Token
varId = satisfy isVarId

conId :: Prod r e Token Token
conId = satisfy isConId

opId :: Prod r e Token Token
opId = satisfy isOpId

mixfixId :: Prod r e Token Token
mixfixId = satisfy isMixfixId

mixfixblkId :: Prod r e Token Token
mixfixblkId = satisfy isMixfixBlkId


var :: Text -> Prod r e Token Token
var txt = satisfy (isVar txt)

con :: Text -> Prod r e Token Token
con txt = satisfy (isCon txt)

op :: Text -> Prod r e Token Token
op txt = satisfy (isOp txt)

mixfix :: Text -> Prod r e Token Token
mixfix txt = satisfy (isMixfix txt)

mixfixblk :: Text -> Prod r e Token Token
mixfixblk txt = satisfy (isMixfixBlk txt)


name :: Prod r e Token Token -> Prod r e Token Name
name p = tokenToName <$> p


-- -----------------------------------------------------------------------------
-- Terminal Productions Helpers for Literal Tokens
tInteger :: Prod r e Token Integer
tInteger = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenInteger _) _) = True
    p  _                             = False
    unsafeExtract (Token (TokenInteger v) _) = v

tReal :: Prod r e Token Double
tReal = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenDouble _) _) = True
    p  _                             = False
    unsafeExtract (Token (TokenDouble v) _) = v

tChar :: Prod r e Token Char
tChar = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenChar _) _) = True
    p  _                             = False
    unsafeExtract (Token (TokenChar v) _) = v

tString :: Prod r e Token String
tString = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenString _) _) = True
    p  _                             = False
    unsafeExtract (Token (TokenString v) _) = v

tBool :: Prod r e Token Bool
tBool = fmap unsafeExtract (satisfy p)
  where
    p (Token (TokenBool _) _) = True
    p  _                             = False
    unsafeExtract (Token (TokenBool v) _) = v


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
  