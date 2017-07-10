{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module Language.Hawk.Syntax.TypeLiteral where

import Data.Binary
import Data.Data
import Data.Text (pack)
import GHC.Generics (Generic)

import Language.Hawk.Syntax.Name

import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- | Type Literal

data TLit
  = TLitInt
  | TLitFloat
  | TLitChar
  | TLitBool
  | TLitData Con
  | TLitFun [TLit] TLit
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)


instance Binary TLit


instance PP.Pretty TLit where
    pretty =
      PP.textStrict . pack . show