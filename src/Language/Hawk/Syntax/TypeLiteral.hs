{-# LANGUAGE DeriveGeneric #-}
module Language.Hawk.Syntax.TypeLiteral where

import Data.Binary
import GHC.Generics (Generic)

import Language.Hawk.Syntax.Name

-- -----------------------------------------------------------------------------
-- | Type Literal

data TLit
  = TLitInt
  | TLitFloat
  | TLitChar
  | TLitBool
  | TLitData Con
  | TLitFun [TLit] TLit
  deriving (Show, Read, Eq, Ord, Generic)


instance Binary TLit