{-# LANGUAGE  DeriveGeneric
            , OverloadedStrings
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.Function where

import Control.Lens
import Data.Aeson
import Data.Binary (Binary)
import Data.Data
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Expression
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Pattern

import qualified Text.PrettyPrint.Leijen.Text as PP


data Fn =
  Fn { _fnName :: Name
     , _fnPats :: [Pat]
     , _fnBody :: Exp
     } deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)


makeClassy ''Fn


instance Binary Fn
instance Plated Fn
instance FromJSON Fn
instance ToJSON Fn


instance PP.Pretty Fn where
  pretty (Fn n xs e) =
    PP.pretty n
      PP.<+> PP.pretty xs
      PP.<+> PP.textStrict "="
      PP.<+> PP.pretty e
