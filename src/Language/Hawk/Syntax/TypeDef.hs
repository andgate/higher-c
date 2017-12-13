{-# LANGUAGE  DeriveGeneric
            , FlexibleContexts
            , TypeFamilies
            , OverloadedStrings
            , LambdaCase
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.TypeDef where

import Control.Lens
import Data.Text (Text)
import Data.Aeson
import Data.Binary (Binary)
import Data.Data
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.Location

import qualified Text.PrettyPrint.Leijen.Text as PP


data TypeDef
  = TypeDef
    { _tdName :: L Text
    , _tdTVars :: [L Text]
    , _tdBody :: Type
    } deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)


instance Binary TypeDef
instance Plated TypeDef
instance FromJSON TypeDef
instance ToJSON TypeDef

instance PP.Pretty TypeDef where
  pretty (TypeDef n tvs t) =
    PP.textStrict "type"
      PP.<+> PP.pretty (unL n)
      PP.<+> PP.pretty (unL <$> tvs)
      PP.<+> PP.textStrict "="
      PP.<+> PP.pretty t
