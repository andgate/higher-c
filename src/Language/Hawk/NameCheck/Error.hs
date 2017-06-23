{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.NameCheck.Error where

import Control.Lens

data NameCheckError
  = SomeNameCheckError
  | UndeclaredVariable

makeClassyPrisms ''NameCheckError