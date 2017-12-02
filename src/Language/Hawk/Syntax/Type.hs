{-# LANGUAGE  TemplateHaskell
            , LambdaCase
            , FlexibleInstances
            , DeriveGeneric
            , DeriveDataTypeable
            , OverloadedStrings
            , StandaloneDeriving
            , MultiParamTypeClasses
  #-}
module Language.Hawk.Syntax.Type where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Data
import Data.Default.Class
import Data.List (concatMap)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Kind

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.Leijen.Text as PP


------------------------------------------------------------------------
-- | Type

data Type
  = TVar  Text
  | TCon  Text
  | TApp  Type Type
  | TArr  Type Type
  | TLoli Type Type
  | TKind Kind Type
  | TLoc  Loc  Type
  | TParen  Type
  | TForall [Text] Type
  deriving (Eq, Ord, Show, Read, Data, Generic, Typeable)


instance Binary Type
instance Plated Type
instance FromJSON Type
instance ToJSON Type


class HasType n where
  typeof :: n -> Type

instance HasType Type where
  typeof = id

instance HasKind Type where
  kind = \case
    TVar _ -> error "unknown kind"
    TCon _ -> error "unknown kind"
    TApp t _ -> case kind t of
                  KArr _ b -> b
                  k        -> k
    TArr _ _  -> KPop
    TLoli _ _ -> KPop
    TKind k _ -> k
    TLoc _ t -> kind t
    TParen t -> kind t

-- -----------------------------------------------------------------------------
-- | "Smart" Constructors

dropForall :: Type -> Type
dropForall =
  transform $ \x -> case x of
    TForall _ t -> t
    t -> t

tcon_ :: Text -> Type
tcon_ = TCon

tFun1 :: Type -> Type -> Type
tFun1 = tArr

tFun2 :: Type -> Type -> Type -> Type
tFun2 a b c = tFun1 a (tFun1 b c)

tLnFun1 :: Type -> Type -> Type
tLnFun1 = tLoli

tLnFun2 :: Type -> Type -> Type -> Type
tLnFun2 a b c = tLnFun1 a (tLnFun1 b c)

tUnit, tInt, tFloat, tChar, tBool :: Type
tUnit  = TKind KPop . TCon $ "()"
tInt   = TKind KPop . TCon $ "Int"
tFloat = TKind KPop . TCon $ "Float"
tChar  = TKind KPop . TCon $ "Char"
tBool  = TKind KPop . TCon $ "Bool"

tArr, tLoli :: Type -> Type -> Type
tArr  = TArr
tLoli = TLoli


-------------------------------------------------------------------------------
-- Helpers

locType :: Type -> Loc
locType = \case
  TVar    _     -> error "Type is not located!"
  TCon    _     -> error "Type is not located!"
  TApp    f  x  -> locType f  <> locType x
  TArr    t1 t2 -> locType t1 <> locType t2
  TLoli   t1 t2 -> locType t1 <> locType t2
  TKind   _  t  -> locType t
  TLoc    l  _  -> l
  TParen  t     -> locType t
  TForall _  t  -> locType t


-- -----------------------------------------------------------------------------
-- | Free Type Variables

class FreeTypeVars a where
  ftv :: a -> Set Text


instance FreeTypeVars Type where
  ftv = \case
    TVar a -> Set.singleton a
    TCon _ -> Set.empty
    TApp t1 t2   -> ftv t1 `Set.union` ftv t2
    TArr t1 t2   -> ftv t1 `Set.union` ftv t2
    TLoli t1 t2  -> ftv t1 `Set.union` ftv t2
    TKind _ t    -> ftv t
    TLoc _ t     -> ftv t
    TParen t     -> ftv t
    TForall ns t -> ftv t `Set.difference` Set.fromList ns


instance FreeTypeVars Text where
  ftv = Set.singleton


instance FreeTypeVars a => FreeTypeVars [a] where
  ftv = foldr (Set.union . ftv) Set.empty


instance (Ord a, FreeTypeVars a) => FreeTypeVars (Set a) where
  ftv = foldr (Set.union . ftv) Set.empty



-- -----------------------------------------------------------------------------
-- | Instances

instance Default Type where
  def = tUnit


instance PP.Pretty Type where
    pretty = \case
      TCon tc ->
        PP.pretty tc

      TVar tv ->
        PP.pretty tv

      TApp f x ->
        PP.pretty f PP.<+> PP.pretty x

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

      TForall n t ->
        PP.textStrict "forall"
          PP.<+> PP.pretty n
          PP.<> PP.textStrict "."
          PP.<+> PP.pretty t
