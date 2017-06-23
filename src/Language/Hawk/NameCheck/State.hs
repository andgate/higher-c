{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.NameCheck.State where

import Control.Lens

data NameCheckState
  = NameCheckState
    { _ncFoo :: Bool
    }

makeClassy ''NameCheckState