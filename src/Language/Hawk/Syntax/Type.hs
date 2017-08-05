{-# LANGUAGE LambdaCase, DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}
module Language.Hawk.Syntax.Type where

import Data.Binary
import Data.Data
import Data.Default.Class
import Data.Text
import GHC.Generics (Generic)

import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Kind

import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- | Type

newtype Tyvar = Tyvar Text
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

newtype Tycon = Tycon Text
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Type
  = TVar Tyvar
  | TCon Tycon
  | TApp Type Type
  | TKind Kind Type
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)


data Scheme = Forall [Tyvar] Type
  deriving (Show, Eq, Ord)

data Qual t =
  [Pred] :=> t
  deriving(Eq)

data Pred
  = IsIn Text [Type]
  deriving(Eq)

-- -----------------------------------------------------------------------------
-- | "Smart" Constructors

tcon_ :: Text -> Type
tcon_ = TCon . Tycon

tFun1 :: Type -> Type -> Type
tFun1 a b = TApp a (TApp tArr b)

tFun2 :: Type -> Type -> Type -> Type
tFun2 a b c = tFun1 a (tFun1 b c)

tLnFun1 :: Type -> Type -> Type
tLnFun1 a b = TApp a (TApp tLoli b)

tLnFun2 :: Type -> Type -> Type -> Type
tLnFun2 a b c = tLnFun1 a (tLnFun1 b c)

tUnit, tInt, tFloat, tChar, tArr, tLoli :: Type
tUnit = TKind KPop . TCon . Tycon $ "()"
tInt = TKind KPop . TCon . Tycon $ "Int"
tFloat = TKind KPop . TCon . Tycon $ "Float"
tChar = TKind KPop . TCon . Tycon $ "Char"

tArr = TKind k . TCon . Tycon $ "(->)"
  where k = KArr KStar (KArr KPop KPop)
tLoli = TKind k . TCon . Tycon $ "(-o)"
  where k = KArr KPop (KArr KPop KPop)



instance HasKind Type where
  kind = \case
    TVar _  -> error "Type without kind"
    TCon _  -> error "Type without kind"
    TApp t _ -> case kind t of
                  (KArr _ k) -> k
                  k          -> k
    TKind k _ -> k


-- -----------------------------------------------------------------------------
-- | Instances

instance Binary Tyvar
instance Binary Tycon
instance Binary Type


instance Default Type where
  def = tUnit

instance PP.Pretty Tyvar where
  pretty (Tyvar n) =  PP.textStrict n

instance PP.Pretty Tycon where
  pretty (Tycon n) =  PP.textStrict n

instance PP.Pretty Type where
    pretty = \case
      TCon tc ->
        PP.pretty tc

      TVar tv ->
        PP.pretty tv

      TApp a b ->
        PP.pretty a PP.<+> PP.pretty b

      TKind k t ->
        PP.pretty t PP.<+> PP.textStrict ":" PP.<+> PP.pretty t
        


instance PP.Pretty Scheme where
  pretty (Forall vs t) =
    PP.textStrict "Forall"
      PP.<+>
      PP.pretty vs
      PP.<>
      PP.textStrict "."
      PP.<+>
      PP.pretty t