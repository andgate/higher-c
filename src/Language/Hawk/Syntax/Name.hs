{-# LANGUAGE  DeriveGeneric
  #-}
module Language.Hawk.Syntax.Name where

import Data.Binary
import Data.Text
import GHC.Generics (Generic)

import qualified Text.PrettyPrint.Leijen.Text as PP

data Name
  = Name Text
  deriving (Eq, Ord, Read, Show, Generic)

data Var
  = Var Text
  deriving (Eq, Ord, Read, Show, Generic)

data Con
  = Con Text
  deriving (Eq, Ord, Read, Show, Generic)

data TVar
  = TypeVar Text
  deriving (Eq, Ord, Read, Show, Generic)



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