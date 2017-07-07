{-# LANGUAGE  TemplateHaskell
  #-}
module Language.Hawk.Parse.Item.LocalState where

import Control.Lens
import Data.Default.Class
import Data.Map (Map)
import Data.Set (Set)
import Language.Hawk.Syntax

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set

data LocalState
  = LocalState
      { _localOps :: Map Var Operator
      , _localTOps :: Map TVar Operator 
      , _localVars :: Set Var
      , _localTVars :: Set TVar
      }

instance Default LocalState where
    def =
      LocalState
        { _localOps = Map.empty
        , _localTOps = Map.empty
        , _localVars = Set.empty
        , _localTVars = Set.empty
        }

makeClassy ''LocalState