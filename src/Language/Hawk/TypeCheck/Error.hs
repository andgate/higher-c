{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.TypeCheck.Error where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data TypeCheckError
  = SomeTypeCheckError
  | TypeMismatchError
  deriving(Show)

makeClassyPrisms ''TypeCheckError

instance Pretty TypeCheckError where
    pretty e = undefined