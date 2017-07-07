{-# LANGUAGE  TemplateHaskell
  #-}
module Language.Hawk.Parse.Item.GlobalInfo where

import Control.Lens
import Data.Map (Map)
import Data.Set (Set)
import Language.Hawk.Syntax

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set

data GlobalInfo
  = GlobalInfo
      { _globalFilePath :: FilePath
      , _globalOps :: Map Var Operator
      , _globalTOps :: Map TVar Operator 
      , _globalVars :: Set Var
      , _globalTVars :: Set TVar
      }

makeClassy ''GlobalInfo
