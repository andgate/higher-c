{-# Language DeriveGeneric, DeriveDataTypeable #-}
module Language.Hawk.Syntax.Decl where

import Data.Data
import Language.Hawk.Syntax.Term
import GHC.Generics (Generic)


data Decl
  = Sig TName Term
  | Def TName Term
  | RecDef TName Term
  deriving (Show, Generic, Typeable)

