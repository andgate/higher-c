{-# LANGUAGE  OverloadedStrings
            , LambdaCase
            , DeriveGeneric
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.CPS.Syntax where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Data
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)


data Program = Program [Def]
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Program
instance Plated Program
instance FromJSON Program
instance ToJSON Program


data Def = Def Text CType CExp
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Def
instance Plated Def
instance FromJSON Def
instance ToJSON Def


data CType
  = Cont CType
  | CInt
  | CPair CType CType
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary CType
instance Plated CType
instance FromJSON CType
instance ToJSON CType


data Lit
  = LInt Int
  | LDouble Double
  | LBool Bool
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Lit
instance Plated Lit
instance FromJSON Lit
instance ToJSON Lit


data AExp
  = CLit Lit
  | CVar Text
  | CLam Text CExp
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary AExp
instance Plated AExp
instance FromJSON AExp
instance ToJSON AExp


data CExp
  = CLet Text Binder CExp
  | CIf AExp CExp CExp
  | CJump AExp AExp
  | CHalt AExp
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary CExp
instance Plated CExp
instance FromJSON CExp
instance ToJSON CExp


data Binder
  = OpBind Text AExp AExp
  | ProjL Text
  | ProjR Text
  | Pair AExp AExp
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Binder
instance Plated Binder
instance FromJSON Binder
instance ToJSON Binder
