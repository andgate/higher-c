{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.Name where

import Data.Binary
import Data.Text
import Data.Data
import GHC.Generics (Generic)

import qualified Text.PrettyPrint.Leijen.Text as PP

data Name
  = Name Text
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

data Var
  = Var Text
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

data Con
  = Con Text
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

data TVar
  = TypeVar Text
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)



instance Binary Name
instance Binary Var
instance Binary Con
instance Binary TVar



instance PP.Pretty Name where
  pretty (Name n) = PP.textStrict n

instance PP.Pretty Var where
  pretty (Var n) = PP.textStrict n

instance PP.Pretty Con where
  pretty (Con n) = PP.textStrict n

instance PP.Pretty TVar where
  pretty (TypeVar n) =  PP.pretty n