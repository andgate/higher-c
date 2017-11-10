{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module Language.Hawk.Syntax.TypeLiteral where

import Data.Aeson
import Data.Binary
import Data.Data
import Data.Text (Text, pack)
import GHC.Generics (Generic)

import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- | Type Literal

data TLit
  = TLitInt
  | TLitFloat
  | TLitChar
  | TLitBool
  | TLitData Text
  | TLitFun [TLit] TLit
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)


instance Binary TLit
instance FromJSON TLit
instance ToJSON TLit


instance PP.Pretty TLit where
    pretty =
      PP.textStrict . pack . show
