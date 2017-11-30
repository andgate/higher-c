{-# LANGUAGE  TemplateHaskell
            , DeriveGeneric
            , OverloadedStrings
  #-}
module Language.Hawk.Syntax.Signature where

import Control.Lens
import Data.Aeson
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Type

import qualified Text.PrettyPrint.Leijen.Text as PP


data Sig
  = Sig
      { _sigName :: Text
      , _sigType :: Type
      }
    deriving (Show, Eq, Generic)


makeClassy ''Sig


instance Binary Sig
instance FromJSON Sig
instance ToJSON Sig


instance PP.Pretty Sig where
  pretty (Sig n t) =
    PP.textStrict n
      PP.<+> PP.textStrict "::"
      PP.<+> PP.pretty t      
