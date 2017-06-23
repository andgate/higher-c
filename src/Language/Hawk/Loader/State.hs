{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Loader.State where

import Control.Lens


data LoaderState
  = LoaderState
    { _loaderBool :: Bool
    }

makeClassy ''LoaderState