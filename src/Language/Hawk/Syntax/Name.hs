{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.Name where

import Data.Binary
import Data.Text
import Data.Data
import GHC.Generics (Generic)

import qualified Text.PrettyPrint.Leijen.Text as PP

newtype Name
  = Name Text
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

newtype Var
  = Var Text
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

newtype Con
  = Con Text
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)



instance Binary Name
instance Binary Var
instance Binary Con



instance PP.Pretty Name where
  pretty (Name n) = PP.textStrict n

instance PP.Pretty Var where
  pretty (Var n) = PP.textStrict n

instance PP.Pretty Con where
  pretty (Con n) = PP.textStrict n

