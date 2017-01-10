{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Language.Hawk.Parse.Helpers where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import qualified Data.ByteString.UTF8 as UTF8
import Text.PrettyPrint.ANSI.Leijen (pretty, Pretty)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

list :: Parser m -> Parser [m] -- header and list items
list i = do
  (i1, is) <- L.nonIndented sc (L.indentBlock sc p)
  return $ i1:is
  where
    p = do
      i1 <- i
      return (L.IndentMany Nothing (return . (i1, )) i)
      
      


-- -----------------------------------------------------------------------------
-- Identifiers
-- 
-- The basic identifiers for each name.

-- A constructor id must start with a lowercase letter, and the body may
-- consist of upper and lowercase letters, numbers, underscores, and tick marks.
varId :: Parser String
varId = 
  lexeme lowNumSymId

-- A constructor id must start with an uppercase letter, and the body may
-- consist of upper and lowercase letters, numbers, underscores, and tick marks.
conId :: Parser String
conId = 
  lexeme capNumSymId


-- A module id must start with an uppercase letter, and the body may
-- consist of upper and lowercase letters
modId :: Parser String
modId = 
  lexeme capId


-- Bases for each id.
capId :: Parser String
capId =
  makeId upperChar letterChar

capNumSymId :: Parser String
capNumSymId =
  makeId upperChar idBodyChar

lowNumSymId :: Parser String
lowNumSymId =
  makeId lowerChar idBodyChar


idBodyChar :: Parser Char
idBodyChar =
  alphaNumChar  <|> char '_' <|> char '`'

-- Make an id parser given a parser for the first character and a body character.
makeId :: Parser Char -> Parser Char -> Parser String
makeId firstChar bodyChar =
  (:) <$> firstChar <*> many bodyChar

-- -----------------------------------------------------------------------------
-- Numbers

integer :: Parser Integer
integer = lexeme L.integer

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float


-- -----------------------------------------------------------------------------
-- Common Symbols


equals :: Parser String
equals =
  symbol "=" <?> "an equals sign '='"
  
objdefsym :: Parser String
objdefsym =
  symbol "^=" <?> "a variable definition symbol '^='"


condefsym :: Parser String
condefsym =
  symbol ":-" <?> "a type definition symbol ':-'"
    
fndefsym :: Parser String
fndefsym =
  symbol ":=" <?> "a function definition symbol ':='"


rightArrow :: Parser String
rightArrow = 
  symbol "->" <?> "an arrow '->'"


hasType :: Parser String
hasType =
  symbol "::" <?> "the \"has type\" symbol '::'"
    
    
comma :: Parser String
comma =
  symbol "," <?> "a comma symbol ','"
  
  
period :: Parser String
period =
  symbol "." <?> "a period symbol '.'"


-- -----------------------------------------------------------------------------
-- Seperated


sep :: Parser a -> Parser [a]
sep = many

commaSep :: Parser a -> Parser [a]
commaSep = 
  (`sepBy` comma)
  
  
rightArrowSep :: Parser a -> Parser [a]
rightArrowSep = 
  (`sepBy` rightArrow)

-- -----------------------------------------------------------------------------
-- Containers

betweenSymbol :: String -> String -> Parser a -> Parser a
betweenSymbol s e =
  between (symbol s) (symbol e)

parens :: Parser a -> Parser a
parens =
  betweenSymbol "(" ")"
  

brackets :: Parser a -> Parser a
brackets =
  betweenSymbol "[" "]"

 
braces :: Parser a -> Parser a
braces =
  betweenSymbol "{" "}"
  

chevrons :: Parser a -> Parser a
chevrons =
  betweenSymbol "<" ">"

qchar :: Parser Char
qchar = lexeme $ 
  between (char '\'') (char '\'') anyChar

qstring :: Parser String
qstring = lexeme $
  between (char '\"') (char '\"') (many $ noneOf "\"")
  

-- -----------------------------------------------------------------------------
-- Location

locate :: Parser a -> Parser (A.Located a)
locate p = do
  p1 <- getPosition
  r <- p
  p2 <- getPosition
  return $ A.A (R.mkRegion p1 p2) r
  
  
withRegion :: Parser a -> (R.Region -> a -> b) -> Parser b
withRegion p func = do
  p1 <- getPosition
  result <- p
  region <- R.mkRegion p1 <$> getPosition
  return $ func region result
  