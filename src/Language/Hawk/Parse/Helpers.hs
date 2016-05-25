{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Hawk.Parse.Helpers where

import Control.Applicative
import Control.Monad.State
import qualified Data.ByteString.UTF8 as UTF8
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta
import qualified Text.Trifecta.Parser as Trifecta
import Text.Trifecta.Result

import Language.Hawk.Parse.Layout
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type IParser a = StateT LayoutEnv Trifecta.Parser a

type MonadicParsing m
  = (DeltaParsing m, LayoutParsing m, Monad m)


defDelta :: String -> Delta
defDelta fn =
  Directed (UTF8.fromString fn) 0 0 0 0


parseFromFile :: MonadIO m => IParser a -> String -> m (Maybe a)
parseFromFile p fn =
  Trifecta.parseFromFile (evalStateT p defLayoutEnv) fn
  

parseFromFileEx :: MonadIO m => IParser a -> String -> m (Result a)
parseFromFileEx p fn =
  Trifecta.parseFromFileEx (evalStateT p defLayoutEnv) fn


parseString :: IParser a -> String -> String -> Result a
parseString p fn str =
  Trifecta.parseString (evalStateT p defLayoutEnv) (defDelta fn) str

-- -----------------------------------------------------------------------------
-- Identifiers
-- 
-- The basic identifiers for each name.

-- A constructor id must start with a lowercase letter, and the body may
-- consist of upper and lowercase letters, numbers, underscores, and tick marks.
varId :: MonadicParsing m => m String
varId = 
  lowNumSymId <?> "a variable id"

-- A constructor id must start with an uppercase letter, and the body may
-- consist of upper and lowercase letters, numbers, underscores, and tick marks.
conId :: MonadicParsing m => m String
conId = 
  capNumSymId <?> "a constructor id"


-- A module id must start with an uppercase letter, and the body may
-- consist of upper and lowercase letters
modId :: MonadicParsing m => m String
modId = 
  capId <?> "a module id"


-- Bases for each id.
capId :: MonadicParsing m => m String
capId =
  makeId upper letter

capNumSymId :: MonadicParsing m => m String
capNumSymId =
  makeId upper $ alphaNum  <|> char '_' <|> char '`'

lowNumSymId :: MonadicParsing m => m String
lowNumSymId =
  makeId lower $ alphaNum  <|> char '_' <|> char '`'


-- Make an id parser given a parser for the first character and a body character.
makeId :: MonadicParsing m => m Char -> m Char -> m String
makeId firstChar bodyChar =
  (:) <$> firstChar <*> many bodyChar 


-- -----------------------------------------------------------------------------
-- Numbers


-- -----------------------------------------------------------------------------
-- Characters




-- -----------------------------------------------------------------------------
-- Common Symbols

equals :: MonadicParsing m => m String
equals =
  string "=" <?> "an equals sign '='"
    

rightArrow :: MonadicParsing m => m String
rightArrow = 
  string "->" <?> "an arrow '->'"


hasType :: MonadicParsing m => m String
hasType =
  string "::" <?> "the \"has type\" symbol '::'"
    
    
comma :: MonadicParsing m => m String
comma =
  (ws *> string "," <* ws) <?> "a comma symbol ','"

-- -----------------------------------------------------------------------------
-- Grouping

sepBy2 :: MonadicParsing m => m a -> m b -> m [a]
sepBy2 p sep =
  (:) <$> p *> sep *> (p `sepBy1` sep)


parens :: MonadicParsing m => m a -> m a
parens p =
  char '(' *> ws *> p <* ws <* char ')'


commaSep :: MonadicParsing m => m a -> m [a]
commaSep p =
  p `sepBy` comma


commaSep1 :: MonadicParsing m => m a -> m [a]
commaSep1 p =
  p `sepBy1` comma
  
  
commaSep2 :: MonadicParsing m => m a -> m [a]
commaSep2 p =
  p `sepBy2` comma
  
-- -----------------------------------------------------------------------------
-- Location

locate :: MonadicParsing m => m a -> m (A.Located a)
locate p = do
  p1 <- position
  r <- p
  p2 <- position
  return $ A.A (R.mkRegion p1 p2) r
  
  
withRegion :: MonadicParsing m => m a -> (R.Region -> a -> b) -> m b
withRegion p func = do
  p1 <- position
  result <- p
  region <- R.mkRegion p1 <$> position
  return $ func region result
  