{-# LANGUAGE  TemplateHaskell
  #-}
module Language.Hawk.Parse.Item.Types where

import Control.Lens
import Data.Default.Class
import Data.Map (Map)
import Data.Set (Set)
import Language.Hawk.Syntax

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set


data GlobalInfo
  = GlobalInfo
      { _gFilePath :: FilePath
      , _gOps   :: Map OpName Operator
      , _gTOps  :: Map OpName Operator
      , _gVars  :: Set Var
      , _gCons  :: Set Con
      }

data LocalState
  = LocalState
      { _localVars  :: Set Var
      }

instance Default LocalState where
    def =
      LocalState
        { _localVars = Set.empty }


makeClassy ''GlobalInfo
makeClassy ''LocalState