{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.SubtermCheck.State where

import Control.Lens
import Data.Default.Class

import Language.Hawk.Syntax.Location


data InferState = InferState { _countfv :: Int }

initInfer :: InferState
initInfer = InferState { _countfv = 0 }


makeClassy ''InferState

instance Default InferState where
  def = 
    InferState
      { _countfv = 0
      }
