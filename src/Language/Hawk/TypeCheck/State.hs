{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.TypeCheck.State where

import Control.Lens
import Data.Default.Class

import Language.Hawk.Syntax.Location


data TCState
  = TCState
    { _tcLocs :: [Loc]
    }

makeClassy ''TCState


instance Default TCState where
  def = 
    TCState
      { _tcLocs = []
      }
