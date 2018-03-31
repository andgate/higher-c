{-# LANGUAGE  FlexibleContexts 
            , TypeFamilies 
            , GeneralizedNewtypeDeriving
            , FlexibleInstances
            , MultiParamTypeClasses
            , TupleSections
            , LambdaCase
  #-}
module Language.Hawk.Parse where

import Control.Lens
import Control.Monad (mapM, (<=<))
import Control.Monad.Extra (mconcatMapM)
import Control.Monad.State (MonadState, execState)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Except
import Control.Monad.Logger
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text, pack)
import Text.Earley (Report (..), Prod)

import Language.Hawk.Lex.Token
import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Grammar
import Language.Hawk.Syntax

import qualified Data.Map.Strict                as Map
import qualified Language.Hawk.Lex.Result       as Lex
import qualified Text.Earley                    as E


parse :: FilePath -> [Token] -> Either ParseError (Lib Term Text)
parse fp toks = do
  let (parses, r@(Report _ expected unconsumed)) = E.fullParses (E.parser toplevel) toks
  case parses of
    []  -> Left $ UnexpectedToken unconsumed
    [p] -> Right p
               
    -- This will only happen if the grammar is wrong
    ps -> Left AmbiguousGrammar