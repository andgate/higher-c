{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.NameCheck.Error where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data NameCheckError
  = SomeNameCheckError
  | UndeclaredVariable
  deriving(Show)

makeClassyPrisms ''NameCheckError

instance Pretty NameCheckError where
    pretty e = undefined