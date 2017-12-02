{-# LANGUAGE LambdaCase, DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}
module Language.Hawk.Syntax.Kind where

import Control.Lens
import Data.Aeson
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


instance HasKind Kind where
  kind = id


ksub :: Kind -> Kind -> Bool
ksub KStar KStar = True
ksub KPop  KPop  = True
ksub KPop  KStar = True
ksub KStar KPop  = False
ksub (KArr a1 a2) (KArr b1 b2) = ksub a1 b1 && ksub a2 b2
ksub _ _ = False


kargs :: Kind -> [Kind]
kargs = \case
  KStar -> []
  KPop -> []
  KArr a b -> a : kargs b


kret :: Kind -> Kind
kret = \case
  KArr _ b -> kret b
  k -> k

instance Binary Kind
instance Plated Kind
instance FromJSON Kind
instance ToJSON Kind


instance PP.Pretty Kind where
    pretty = \case
      KStar -> PP.textStrict "*"

      KPop -> PP.textStrict "o"

      KArr a b ->
        PP.pretty a PP.<+> PP.textStrict "->" PP.<+> PP.pretty b
