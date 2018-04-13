{-# LANGUAGE  FlexibleContexts 
            , TypeFamilies 
            , GeneralizedNewtypeDeriving
            , FlexibleInstances
            , MultiParamTypeClasses
            , TupleSections
            , LambdaCase
  #-}
module Language.Hawk.Parse where

import Text.Earley (Report (..), Prod)

import Language.Hawk.Lex.Token
import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Grammar
import Language.Hawk.Syntax.Source

import qualified Text.Earley as E


parse :: FilePath -> [Token] -> Either ParseError TopLevelDef
parse fp toks = do
  let (parses, r@(Report _ expected unconsumed)) = E.fullParses (E.parser toplevel) toks
  case parses of
    []  -> Left $ UnexpectedToken unconsumed
    [p] -> Right p
               
    -- This will only happen if the grammar is wrong
    ps -> Left AmbiguousGrammar