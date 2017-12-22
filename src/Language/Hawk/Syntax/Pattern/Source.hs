{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
            , DeriveTraversable
            , MonadComprehensions
            , OverloadedStrings
  #-}
module Language.Hawk.Syntax.Pattern.Source where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Data
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Name

------------------------------------------------------------------------------
-- Source Pattern
data Pat t b
  = PVar NameHint b
  | PWild
  | PLit Lit
  | PCon Text [Pat t b]
  | PAnnot t (Pat t b)
  | PView t (Pat t b)
  | PLoc Loc (Pat t b)
  deriving(Show,Read,Ord,Eq,Data,Typeable,Generic)

-- Pattern instances
instance (Binary t, Binary b) => Binary (Pat t b)
instance (Data t, Data b) => Plated (Pat t b)
instance (FromJSON t, FromJSON b) => FromJSON (Pat t b)
instance (ToJSON t, ToJSON b) => ToJSON (Pat t b)