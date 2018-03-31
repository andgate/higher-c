{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.TypeCheck.State where

import Control.Lens
import Data.Default.Class
import Data.Map.Strict (Map)
import Language.Hawk.Syntax.Location

import qualified Data.Map.Strict as Map


data TypeState
  = TypeState
      { _countfv :: Int
      }


makeClassy ''TypeState

instance Default TypeState where
  def = 
    TypeState
      { _countfv = 0
      }
