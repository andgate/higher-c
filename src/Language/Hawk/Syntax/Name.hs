{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
            , LambdaCase
  #-}
module Language.Hawk.Syntax.Name where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Data
import Data.Text
import GHC.Generics (Generic)

import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.Kind

import qualified Text.PrettyPrint.Leijen.Text as PP

data Name
  = Name Text
  | NLoc Loc Name
  | NType Type Name
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Binary Name
instance Plated Name
instance FromJSON Name
instance ToJSON Name


instance HasKind Name where
  kind = \case
    Name _ -> error "Cannot extract kind from name without type information"
    NLoc _ n -> kind n
    NType t _ -> kind t


instance PP.Pretty Name where
  pretty = \case
    Name n -> PP.textStrict n
    NLoc _ n -> PP.pretty n
    NType _ n -> PP.pretty n


-------------------------------------------------------------------------
-- Helpers

locName :: Name -> Maybe Loc
locName = \case
  Name _ -> Nothing
  NLoc l _ -> Just l
  NType _ n -> locName n

locName' :: Name -> Loc
locName' = \case
  Name _ -> error "Name has no location embedded."
  NLoc l _ -> l
  NType _ n -> locName' n

  
typeName :: Name -> Maybe Type
typeName = \case
  Name _ -> Nothing
  NLoc _ n -> typeName n
  NType t _ -> Just t
  

typeName' :: Name -> Type
typeName' = \case
  Name _ -> error "Name has no type embedded."
  NLoc _ n -> typeName' n
  NType t _ -> t


readName :: Name -> Text
readName = \case
  Name n -> n
  NLoc _ n -> readName n
  NType _ n -> readName n
