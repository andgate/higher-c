{-# LANGUAGE  OverloadedStrings
            , LambdaCase
            , DeriveGeneric
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.CPS.Syntax
  ( module Language.CPS.Syntax
  , module Language.Hawk.Syntax.Prim
  )
where

import Control.Lens
import Data.Aeson hiding (Value)
import Data.Binary
import Data.Data
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Prim

import qualified Data.Set as Set


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


data Struct = Struct Text Integer
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Struct
instance Plated Struct
instance FromJSON Struct
instance ToJSON Struct


data Lambda = Lambda
  { _lamName :: Text
  , _lamTerm :: Term
  }
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

instance Binary Lambda
instance Plated Lambda
instance FromJSON Lambda
instance ToJSON Lambda


data Program = Program 
  { _cpsLambdas :: [Lambda]
  , _cpsStructs :: [Struct]
  , _cpsExterns :: Set Text
  , _cpsGlobals :: Set Text
  }
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)

makeClassy ''Program

instance Binary Program
instance Plated Program
instance FromJSON Program
instance ToJSON Program

instance Monoid Program where
  mempty = Program [] [] Set.empty Set.empty
  mappend p1 p2 =
    p1 & (cpsLambdas <>~ (p2^.cpsLambdas))
       . (cpsStructs <>~ (p2^.cpsStructs))
       . (cpsExterns <>~ (p2^.cpsExterns))
       . (cpsGlobals <>~ (p2^.cpsGlobals))

