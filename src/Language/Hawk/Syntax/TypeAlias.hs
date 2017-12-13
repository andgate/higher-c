{-# LANGUAGE  DeriveGeneric
            , FlexibleContexts
            , TypeFamilies
            , OverloadedStrings
            , LambdaCase
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.TypeAlias where

import Control.Lens
import Data.Text (Text)
import Data.Aeson
import Data.Binary (Binary)
import Data.Data
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.Location

import qualified Text.PrettyPrint.Leijen.Text as PP


data TypeAlias
  = TypeAlias
    { _taName :: L Text
    , _taTVars :: [L Text]
    , _taBody :: Type
    } deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)


instance Binary TypeAlias
instance Plated TypeAlias
instance FromJSON TypeAlias
instance ToJSON TypeAlias

instance PP.Pretty TypeAlias where
  pretty (TypeAlias n tvs t) =
    PP.textStrict "alias"
      PP.<+> PP.pretty (unL n)
      PP.<+> PP.pretty (unL <$> tvs)
      PP.<+> PP.textStrict "="
      PP.<+> PP.pretty t
