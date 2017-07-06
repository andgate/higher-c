{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.TypeCheck.Error where

import Control.Lens
import Language.Hawk.Syntax (Type, TVar, Var)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data TypeCheckError
  = UnificationFailure Type Type
  | OccursCheckFail TVar Type
  | UnboundVariable Var
  deriving(Show)

makeClassyPrisms ''TypeCheckError

instance Pretty TypeCheckError where
    pretty e = undefined