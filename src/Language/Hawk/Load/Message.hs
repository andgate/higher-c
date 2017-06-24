{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.Load.Message where

import Control.Lens
import System.FilePath (FilePath)

import Language.Hawk.Load.Error

data LoadMsg
  = FileFound FilePath
  | LoadErrMsg LoadErr
  deriving(Show)

makeClassyPrisms ''LoadMsg


instance AsLoadErr LoadMsg where
    _LoadErr = _LoadErrMsg