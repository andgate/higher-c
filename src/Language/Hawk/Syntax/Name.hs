{-# LANGUAGE  LambdaCase
            , DeriveGeneric
            , OverloadedStrings
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.Name where

import Control.Lens
import Data.Aeson
import Data.Binary (Binary)
import Data.Data
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Location

import qualified Text.PrettyPrint.Leijen.Text as PP

data NameHint = N Text | Nameless
  deriving (Eq, Ord, Show)

data Name t b
  = Name b
  | NLoc Loc (Name t b)
  | NTerm t (Name t b)
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)


instance (Binary t, Binary b) => Binary (Name t b)
instance (Data t, Plated t, Data b, Plated b) => Plated (Name t b)
instance (FromJSON t, FromJSON b) => FromJSON (Name t b)
instance (ToJSON t, ToJSON b) => ToJSON (Name t b)


-- -----------------------------------------------------------------------------
-- | Default Instances

instance Default b => Default (Name t b) where
  def = Name def

--------------------------------------------------------------------------------
-- Helpers

mkLocName :: (b, Loc) -> Name t b
mkLocName (n, l) = NLoc l $ Name n


locName :: Name t b -> Maybe Loc
locName = \case
  Name _ -> Nothing
  NLoc l _ -> Just l
  NTerm _ n -> locName n

locName' :: Name t b -> Loc
locName' = \case
  Name _ -> error "Name has no embedded location."
  NLoc l _ -> l
  NTerm _ n -> locName' n

  
typeName :: Name t b -> Maybe t
typeName = \case
  Name _ -> Nothing
  NLoc _ n -> typeName n
  NTerm t _ -> Just t
  

typeName' :: Name t b -> t
typeName' = \case
  Name _ -> error "Name has no embedded term."
  NLoc _ n -> typeName' n
  NTerm t _ -> t


readName :: Name t b -> b
readName = \case
  Name n -> n
  NLoc _ n -> readName n
  NTerm _ n -> readName n


-- -----------------------------------------------------------------------------
-- | Pretty Printing

instance (PP.Pretty t, PP.Pretty b) => PP.Pretty (Name t b) where
  pretty = \case
    Name n -> PP.pretty n
    NLoc _ n -> PP.pretty n
    NTerm _ n -> PP.pretty n