{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.NameCheck.State where

import Control.Lens
import Data.Default.Class
import Data.Text


data NameFrame = NameFrame [Text]
data NameStack = NameStack [NameFrame]

data NcState
  = NcState
    { _ncstk :: NameStack
    }

makeClassy ''NcState


instance Default NcState where
    def =
      NcState
        { _ncstk = NameStack []
        }
