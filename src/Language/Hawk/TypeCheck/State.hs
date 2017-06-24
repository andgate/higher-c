{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.TypeCheck.State where

import Control.Lens
import Data.Default.Class


data TypeCheckState
  = TypeCheckState
    { _tcFoo :: Bool
    }

makeClassy ''TypeCheckState


instance Default TypeCheckState where
  def = 
    TypeCheckState
      { _tcFoo = False
      }