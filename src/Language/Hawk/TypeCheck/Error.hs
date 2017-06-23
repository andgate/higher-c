{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.TypeCheck.Error where

import Control.Lens

data TypeCheckError
  = SomeTypeCheckError
  | TypeMismatchError

makeClassyPrisms ''TypeCheckError