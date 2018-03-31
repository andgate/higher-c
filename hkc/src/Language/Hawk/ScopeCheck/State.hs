{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.ScopeCheck.State where

import Control.Lens
import Data.Default.Class
import Data.Text


newtype ScopeFrame = ScopeFrame [Text]
newtype ScopeStack = ScopeStack [ScopeFrame]

newtype ScState = ScState
  { _scstk :: ScopeStack
  }

makeClassy ''ScState


instance Default ScState where
    def =
      ScState
        { _scstk = ScopeStack []
        }
