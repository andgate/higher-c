{-# LANGUAGE TemplateHaskell, LambdaCase, DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}
module Language.Hawk.Syntax.Type where

import Control.Lens
import Data.Binary
import Data.Data
import Data.Default.Class
import Data.List (concatMap)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import GHC.Generics (Generic)

import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Kind

import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- | Type

data K = K Kind Type

data Type
  = TVar Text
  | TCon Text
  | TApp Type Type
  | TArr Type Type
  | TLoli Type Type
  | TKind Kind Type
  | TLoc Loc Type
  | TParen Type
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)


makeClassyPrisms ''Type

data Scheme = Forall [Text] Type
  deriving (Show, Eq, Ord)


-- -----------------------------------------------------------------------------
-- | "Smart" Constructors

tcon_ :: Text -> Type
tcon_ = TCon

tFun1 :: Type -> Type -> Type
tFun1 a b = TApp a (TApp tArr b)

tFun2 :: Type -> Type -> Type -> Type
tFun2 a b c = tFun1 a (tFun1 b c)

tLnFun1 :: Type -> Type -> Type
tLnFun1 a b = TApp a (TApp tLoli b)

tLnFun2 :: Type -> Type -> Type -> Type
tLnFun2 a b c = tLnFun1 a (tLnFun1 b c)

tUnit, tInt, tFloat, tChar, tArr, tLoli :: Type
tUnit  = TKind KPop . TCon $ "()"
tInt   = TKind KPop . TCon $ "Int"
tFloat = TKind KPop . TCon $ "Float"
tChar  = TKind KPop . TCon $ "Char"

tArr  = TKind k . TCon $ "(->)"
  where k = KArr KStar (KArr KPop KPop)
tLoli = TKind k . TCon $ "(-o)"
  where k = KArr KPop (KArr KPop KPop)


instance HasKind Type where
  kind = \case
    TVar _  -> error "Type without kind"
    TCon _  -> error "Type without kind"
    TApp t _ -> case kind t of
                  (KArr _ k) -> k
                  k          -> k
    TKind k _ -> k
    TLoc _ t  -> kind t


-- -----------------------------------------------------------------------------
-- | Instances

instance Binary Type


instance Default Type where
  def = tUnit


instance PP.Pretty Type where
    pretty = \case
      TCon tc ->
        PP.pretty tc

      TVar tv ->
        PP.pretty tv

      TApp a b ->
        PP.pretty a PP.<+> PP.pretty b

      TArr a b ->
        PP.pretty a PP.<+> PP.textStrict "->" PP.<+> PP.pretty b

      TLoli a b ->
        PP.pretty a PP.<+> PP.textStrict "-o" PP.<+> PP.pretty b

      TKind k t ->
        PP.pretty t PP.<+> PP.textStrict ":" PP.<+> PP.pretty k

      TLoc l t ->
        PP.pretty t

      TParen t ->
        PP.parens $ PP.pretty t
        


instance PP.Pretty Scheme where
  pretty (Forall vs t) =
    PP.textStrict "Forall"
      PP.<+>
      PP.pretty vs
      PP.<>
      PP.textStrict "."
      PP.<+>
      PP.pretty t
