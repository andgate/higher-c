{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Hawk.Parse.Helpers where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import qualified Data.ByteString.UTF8 as UTF8
import Text.PrettyPrint.ANSI.Leijen (pretty, Pretty)
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


-- -----------------------------------------------------------------------------
-- Parser type

type HkParser a = ReaderT (Parser ()) Parser a



parseString :: HkParser a -> String -> String -> Either (ParseError (Token String) Dec) a
parseString p n str = 
  parse (runReaderT p scDef) n str


scDef :: Parser ()
scDef = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

thisOr :: HkParser a -> HkParser a -> HkParser a
thisOr p q = try p <|> q


lexeme :: HkParser a -> HkParser a
lexeme p = do
  sc <- lift <$> ask
  L.lexeme sc p
  
symbol :: String -> HkParser String
symbol str = do
  sc <- lift <$> ask
  L.symbol sc str
  

list :: HkParser a -> HkParser [a] -- header and list items
list i = do
  sc <- ask
  (i1, is) <- (L.indentBlock sc p) :: Parser (a, [a])
  return $ i1:is
  where
    p = do
      i1 <- i
      return (L.IndentSome Nothing (return . (i1, )) i)
      
lineFold :: HkParser a -> HkParser a
lineFold p = do
  sc <- ask
  L.lineFold sc $
      \sc' -> local (\sc -> thisOr sc' sc) p
      
      
nonIndented :: HkParser a -> HkParser a
nonIndented =
  withSpace L.nonIndented
  
withSpace :: (HkParser () -> HkParser a -> HkParser a) -> HkParser a -> HkParser a
withSpace f p = 
  ask >>= (flip f) p



-- -----------------------------------------------------------------------------
-- Identifiers
-- 
-- The basic identifiers for each name.

-- A constructor id must start with a lowercase letter, and the body may
-- consist of upper and lowercase letters, numbers, underscores, and tick marks.
varId :: HkParser String
varId = 
  lexeme lowNumSymId

-- A constructor id must start with an uppercase letter, and the body may
-- consist of upper and lowercase letters, numbers, underscores, and tick marks.
conId :: HkParser String  
conId = 
  lexeme capNumSymId


-- A module id must start with an uppercase letter, and the body may
-- consist of upper and lowercase letters
modId :: HkParser String
modId = 
  lexeme capId


-- Bases for each id.
capId :: HkParser String
capId =
  makeId upperChar letterChar

capNumSymId :: HkParser String
capNumSymId =
  makeId upperChar idBodyChar

lowNumSymId :: HkParser String
lowNumSymId =
  makeId lowerChar idBodyChar


idBodyChar :: HkParser Char
idBodyChar =
  alphaNumChar  <|> char '_' <|> char '`'

-- Make an id parser given a parser for the first character and a body character.
makeId :: HkParser Char -> HkParser Char -> HkParser String
makeId firstChar bodyChar =
  (:) <$> firstChar <*> many bodyChar

-- -----------------------------------------------------------------------------
-- Numbers

integer :: HkParser Integer
integer = lexeme L.integer

float :: HkParser Double
float  = lexeme L.float

signedInteger :: HkParser Integer
signedInteger = L.signed (pure ()) integer

signedFloat :: HkParser Double
signedFloat = L.signed (pure ()) float


-- -----------------------------------------------------------------------------
-- Common Symbols


equals :: HkParser String
equals =
  symbol "=" <?> "an equals sign '='"
  
vardefsym :: HkParser String
vardefsym =
  symbol "^=" <?> "a variable definition symbol '^='"


condefsym :: HkParser String
condefsym =
  symbol ":-" <?> "a type definition symbol ':-'"
    
fndefsym :: HkParser String
fndefsym =
  symbol ":=" <?> "a function definition symbol ':='"


rightArrow :: HkParser String
rightArrow = 
  symbol "->" <?> "an arrow '->'"


hasType :: HkParser String
hasType =
  symbol "::" <?> "the \"has type\" symbol '::'"
    
    
comma :: HkParser String
comma =
  symbol "," <?> "a comma symbol ','"
  
  
period :: HkParser String
period =
  symbol "." <?> "a period symbol '.'"


-- -----------------------------------------------------------------------------
-- Seperated


sep :: HkParser a -> HkParser [a]
sep = many

commaSep :: HkParser a -> HkParser [a]
commaSep = 
  (`sepBy` comma)
  
  
rightArrowSep :: HkParser a -> HkParser [a]
rightArrowSep = 
  (`sepBy` rightArrow)

-- -----------------------------------------------------------------------------
-- Containers

betweenSymbol :: String -> String -> HkParser a -> HkParser a
betweenSymbol s e =
  between (symbol s) (symbol e)

parens :: HkParser a -> HkParser a
parens =
  betweenSymbol "(" ")"
  

brackets :: HkParser a -> HkParser a
brackets =
  betweenSymbol "[" "]"

 
braces :: HkParser a -> HkParser a
braces =
  betweenSymbol "{" "}"
  

chevrons :: HkParser a -> HkParser a
chevrons =
  betweenSymbol "<" ">"

qchar :: HkParser Char
qchar = lexeme $ 
  between (char '\'') (char '\'') anyChar

qstring :: HkParser String
qstring = lexeme $
  between (char '\"') (char '\"') (many $ noneOf "\"")
  

-- -----------------------------------------------------------------------------
-- Location

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
  