{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.Loader.Error where

import Control.Lens
import System.FilePath

data LoaderError
  = BadModuleName FilePath
  | SomeLoaderError -- Placeholder until more errors are needed

makeClassyPrisms ''LoaderError