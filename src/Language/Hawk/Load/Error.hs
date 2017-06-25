{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.Load.Error where

import Control.Lens
import System.FilePath

data LoadErr
  = FileNotFound FilePath
  | FileInUse FilePath
  | PermissionDenied FilePath
  | UndefinedLoadErr FilePath
  deriving(Show)

makeClassyPrisms ''LoadErr