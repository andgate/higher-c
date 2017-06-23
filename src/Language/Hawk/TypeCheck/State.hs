{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.TypeCheck.State where

import Control.Lens

data TypeCheckState
  = TypeCheckState
    { _tcFoo :: Bool
    }

makeClassy ''TypeCheckState