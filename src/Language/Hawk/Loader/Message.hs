{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.Loader.Message where

import Control.Lens
import System.FilePath (FilePath)

import Language.Hawk.Loader.Error

data LoaderMessage
  = FileFound FilePath
  | DirectoryFound FilePath
  | WarnFileIgnored FilePath
  | WarnDirectoryIgnored FilePath
  | WarnSymLinkIgnored FilePath
  | LoaderErrMsg LoaderError

makeClassyPrisms ''LoaderMessage


instance AsLoaderError LoaderMessage where
    _LoaderError = _LoaderErrMsg