{-# LANGUAGE ConstraintKinds #-}
module Language.Hawk.Parse.Helpers where

import Control.Applicative
import Data.Tuple (snd)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R



type MonadicParsing m
  = (DeltaParsing m, Monad m)

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
    
    
    
instance R.HasPosition Delta where
  getPosition (Columns _ _) =
    error "Delta Columns constructor does not have position"
    
  getPosition (Tab _ _ _) =
    error "Delta Tab constuctor does not have position"
    
  getPosition (Lines line column _ _) =
    R.Position line column
    
  getPosition (Directed _ line column _ _) =
    R.Position line column
    


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
  