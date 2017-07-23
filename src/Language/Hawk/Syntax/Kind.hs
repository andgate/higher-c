{-# LANGUAGE LambdaCase, DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}
module Language.Hawk.Syntax.Kind where

import Data.Binary
import Data.Data
import GHC.Generics (Generic)

import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- | Kind type

data Kind
  = KStar
  | KPop
  | KArr Kind Kind
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)


class HasKind t where
  kind :: t -> Kind

instance Binary Kind


instance PP.Pretty Kind where
    pretty = \case
      KStar -> PP.textStrict "*"

      KPop -> PP.textStrict "o"

      KArr a b ->
        PP.pretty a PP.<+> PP.textStrict "->" PP.<+> PP.pretty b