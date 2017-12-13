{-# LANGUAGE  OverloadedStrings
            , LambdaCase
            , DeriveGeneric
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.CPS.Syntax
  ( module Language.Hawk.CPS.Syntax
  , module Language.Hawk.Syntax.Prim
  )
where

import Control.Lens
import Data.Aeson hiding (Value)
import Data.Binary
import Data.Data
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Prim


data Program = Program [Def]
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Program
instance Plated Program
instance FromJSON Program
instance ToJSON Program


data Def = Def Text Term
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Def
instance Plated Def
instance FromJSON Def
instance ToJSON Def


data Lit
  = LInt Int
  | LDouble Double
  | LBool Bool
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Lit
instance Plated Lit
instance FromJSON Lit
instance ToJSON Lit


data Value
  = Lit Lit
  | Var Text
  | Use Text
  | Dup Text
  | Lam Text Term
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Value
instance Plated Value
instance FromJSON Value
instance ToJSON Value


data Term
  = Let Text Binder Term
  | If Value Term Term
  | Free [Text] Term
  | Jump Value Value
  | Halt Value
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Term
instance Plated Term
instance FromJSON Term
instance ToJSON Term


data Binder
  = PrimBind PrimInstr Value Value
  | ProjL Text
  | ProjR Text
  | Pair Value Value
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Binder
instance Plated Binder
instance FromJSON Binder
instance ToJSON Binder
