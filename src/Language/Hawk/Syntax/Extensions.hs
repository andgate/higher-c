{-# LANGUAGE  DataKinds
            , ConstraintKinds
            , KindSignatures
            , TypeFamilies
            , FlexibleContexts
  #-}
module Language.Hawk.Syntax.Extensions where

import Data.Binary
import Data.Default.Class
import GHC.Types (Constraint)
import GHC.Generics (Generic)
import Text.PrettyPrint.Leijen.Text (Pretty)


-- -----------------------------------------------------------------------------
-- | Module Scope

type ForallX (c :: * -> Constraint) (x :: *)
  = ( ForallExp c x
    , c (XMScope x)
    , c (XDef x)
    , c (XPat x)
    )

type GenericX (x :: *)
  = ForallX Generic x

type ShowX (x :: *)
  = ForallX Show x

type EqX (x :: *)
  = ForallX Eq x

type OrdX (x :: *)
  = ForallX Ord x

type PrettyX (x :: *)
  = ForallX Pretty x

type BinaryX (x :: *)
  = (ForallX Binary x, GenericX x)

type DefaultX (x :: *)
  = (ForallX Default x)

-- -----------------------------------------------------------------------------
-- | Module Scope Extension

type family XMScope x


-- -----------------------------------------------------------------------------
-- | Definition Extension

type family XDef x

-- -----------------------------------------------------------------------------
-- | Pattern Extension

type family XPat x

-- -----------------------------------------------------------------------------
-- | Expression Extensions

type family XELit x
type family XEVar x
type family XECon x
type family XEApp x
type family XELam x
type family XELet x
type family XEDup x
type family XEDrop x
type family XEPrim x
type family XEIf x
type family XETypeHint x
type family XExp x

type ForallExp (c :: * -> Constraint) (x :: *) =
  ( c (XELit x)
  , c (XEVar x)
  , c (XECon x)
  , c (XEPrim x)
  , c (XEApp x)
  , c (XELam x)
  , c (XEIf x)
  , c (XELet x)
  , c (XEDup x)
  , c (XEDrop x)
  , c (XETypeHint x)
  , c (XExp x)
  )