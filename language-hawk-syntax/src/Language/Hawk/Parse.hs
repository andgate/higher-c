{-# LANGUAGE  FlexibleContexts 
            , TypeFamilies 
            , GeneralizedNewtypeDeriving
            , FlexibleInstances
            , MultiParamTypeClasses
            , TupleSections
            , LambdaCase
  #-}
module Language.Hawk.Parse where

import Data.Either (partitionEithers)
import Text.Earley (Report (..), Prod)

import Language.Hawk.Lex.Token
import Language.Hawk.Lex.LFCut
import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Grammar
import Language.Hawk.Syntax.Source

import qualified Text.Earley as E


parse :: FilePath -> [Token] -> ([ParseError], [TopLevelDef])
parse fp toks =
  let toks' = lfCut toks
  in partitionEithers (parseDef fp <$> toks')

parseDef :: FilePath -> [Token] -> Either ParseError TopLevelDef
parseDef fp toks = do
  let (parses, r@(Report _ expected unconsumed)) = E.fullParses (E.parser toplevel) toks
  case parses of
    []  -> Left $ UnexpectedToken unconsumed expected
    [p] -> Right p
               
    -- This will only happen if the grammar is wrong
    ps -> Left $ AmbiguousGrammar ps