{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.NameCheck.State where

import Control.Lens
import Data.Default.Class

data NameCheckState
  = NameCheckState
    { _ncFoo :: Bool
    }

makeClassy ''NameCheckState


instance Default NameCheckState where
    def =
      NameCheckState
        { _ncFoo = False
        }