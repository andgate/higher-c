{-# LANGUAGE LambdaCase, DeriveGeneric, OverloadedStrings #-}
module Language.Hawk.Syntax.Type where

import Data.Binary
import Data.Default.Class
import Data.Text
import GHC.Generics (Generic)

import Language.Hawk.Syntax.Name

import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- | Type

data Type
  = TCon Con
  | TVar TVar
  | TFun Type Type
  deriving (Show, Eq, Generic)

-- -----------------------------------------------------------------------------
-- | Instances

instance Binary Type

instance Default Type where
  def = TCon $ Con "()"


instance PP.Pretty Type where
    pretty = \case
      TCon n ->
        PP.textStrict "TCon:" PP.<+> PP.pretty n

      TVar tvar ->
        PP.pretty tvar

      TFun f x ->
        PP.textStrict "TFun:"
        PP.<$>
        PP.indent 2
          ( PP.textStrict "func:" PP.<+> PP.pretty f
            PP.<$>
            PP.textStrict "arg:" PP.<$> PP.pretty x
          )