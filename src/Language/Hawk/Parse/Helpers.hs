{-# LANGUAGE  OverloadedStrings
            , ConstraintKinds
            , FlexibleContexts
            , FlexibleInstances
            , TypeFamilies
            , RankNTypes
  #-}
module Language.Hawk.Parse.Helpers where

import Control.Applicative
import Control.Monad (void)
import Data.Text (Text, pack)
import Data.Void (Void)
import Language.Hawk.Syntax

import Text.Megaparsec (MonadParsec(..))

import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as L


-- -----------------------------------------------------------------------------
-- Parser Monad Class

type MonadParser m = (Functor m, Applicative m, Monad m, MonadParsec Void String m)

type Parser = P.Parsec Void String


-- -----------------------------------------------------------------------------
-- Comments and Whitespace

lnCmt :: MonadParser m => m ()
lnCmt = L.skipLineComment "--"

blkCmt :: MonadParser m => m ()
blkCmt = L.skipBlockCommentNested "--|" "|--"

-- Whitespace w/ newline
scn :: MonadParser m => m ()
scn = L.space P.space1 lnCmt blkCmt


-- Whitespace w/out newline
sc :: MonadParser m => m ()
sc = L.space (void $ takeWhile1P Nothing f) lnCmt blkCmt
  where
    f x = x == ' ' || x == '\t'


-- -----------------------------------------------------------------------------
-- Reserved words and Identifiers

reserved :: [String]
reserved = [ "let", "in"
           , "free", "copy"
           , "if", "then", "else"
           , "infix", "infixl", "infixr"
           ]

reservedOps :: [String]
reservedOps =
  [ "="
  , ":"
  ]

rsvp :: MonadParser m => m () -> String -> m ()
rsvp ws w = L.lexeme ws (P.string w *> P.notFollowedBy P.alphaNumChar)

symbol :: MonadParser m => m () -> String -> m String
symbol = L.symbol 

varName :: MonadParser m => m () -> m Text
varName ws = pack <$> varid ws

conName :: MonadParser m => m () -> m Text
conName ws = pack <$> conid ws

opName :: MonadParser m => m () -> m Text
opName ws = pack <$> opid ws
  

varid :: MonadParser m => m () -> m String
varid ws = (L.lexeme ws . try) (p >>= check)
  where
    p       = (:) <$> P.lowerChar <*> many (P.alphaNumChar <|> P.char '_' <|> P.char '\'')
    check x = if x `elem` reserved
                then fail $ "keyword " ++ show x ++ " cannot be an variable identifier"
                else return x


conid :: MonadParser m => m () -> m String
conid ws = (L.lexeme ws . try) p
  where
    p       = (:) <$> P.upperChar <*> many (P.alphaNumChar <|> P.char '_' <|> P.char '\'')


opid :: MonadParser m => m () -> m String
opid ws = (L.lexeme ws . try) (p >>= check)
  where
    p       = some P.symbolChar
    check x = if x `elem` reservedOps
                then fail $ "keyword " ++ show x ++ " is reserved and cannot be an operator"
                else return x
                     

-- -----------------------------------------------------------------------------
-- Symbols

parens :: MonadParser m => m () -> m a -> m a
parens ws = P.between (L.symbol ws "(") (L.symbol ws ")")

braces :: MonadParser m => m () -> m a -> m a
braces ws = P.between (L.symbol ws "{") (L.symbol ws "}")

angles :: MonadParser m => m () -> m a -> m a
angles ws = P.between (L.symbol ws "<") (L.symbol ws ">")

brackets :: MonadParser m => m () -> m a -> m a
brackets ws = P.between (L.symbol ws "[") (L.symbol ws "]")

semicolon :: MonadParser m => m () -> m String
semicolon ws = L.symbol ws ";"

comma :: MonadParser m => m () -> m String
comma ws = L.symbol ws ","

colon :: MonadParser m => m () -> m String
colon ws = L.symbol ws ":"

dot :: MonadParser m => m () -> m String
dot ws = L.symbol ws "."

equals :: MonadParser m => m () -> m String
equals ws = L.symbol ws "="

arrowr :: MonadParser m => m () -> m String
arrowr ws = L.symbol ws "->"

arrowl :: MonadParser m => m () -> m String
arrowl ws = L.symbol ws "<-"


backslash :: MonadParser m => m () -> m String
backslash ws = L.symbol ws "\\"


commaSep1 :: MonadParser m => m () -> m a -> m [a]
commaSep1 ws p = P.sepBy1 p (comma ws) 

-- -----------------------------------------------------------------------------
-- Literals

integerP :: MonadParser m => m () -> m Integer
integerP ws = L.lexeme ws L.decimal

doubleP :: MonadParser m => m () -> m Double
doubleP ws = L.lexeme ws L.float

charP :: MonadParser m => m () -> m Char
charP ws =
  L.lexeme ws $
    P.char '\'' *>  L.charLiteral <* P.char '\''

stringP' :: MonadParser m => m () -> m Text
stringP' ws =
  pack <$> stringP ws

stringP :: MonadParser m => m () -> m String
stringP ws =
  L.lexeme ws $
    P.char '"' >> P.manyTill L.charLiteral (P.char '"')
           
-- -----------------------------------------------------------------------------
-- Location Helpers


peekPos :: MonadParser m => m (FilePath, Position)
peekPos = do
  P.SourcePos fp ln col <- P.getPosition
  let ln' = P.unPos ln
      col' = P.unPos col
  return (fp, P ln' col')


locP :: MonadParser m
     => m a -> m (L a)
locP m = do
  (fp, p1) <- peekPos
  x <- m
  withRecovery (\_ -> return $ L (Loc fp $ R p1 p1) x) $ do
    (_, p2) <- peekPos
    return $ L (Loc fp $ R p1 p2) x

-- | Wrap an expression with a location
elocP :: MonadParser m
         => m Exp -> m Exp
elocP m = do
  (fp, p1) <- peekPos
  t <- m
  withRecovery (\_ -> return $ ELoc (Loc fp $ R p1 p1) t) $ do
    (_, p2) <- peekPos
    return $ ELoc (Loc fp $ R p1 p2) t
