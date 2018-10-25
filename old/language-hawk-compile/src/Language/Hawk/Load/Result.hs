{-# LANGUAGE  DeriveGeneric, TemplateHaskell #-}
module Language.Hawk.Load.Result where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Default.Class
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Map.Strict as Map
import qualified Text.PrettyPrint.Leijen.Text as PP


-------------------------------------------------------------------------------
-- Load Result
-------------------------------------------------------------------------------

data LdResult
  = LdResult
    { _ldFiles :: Map FilePath Text
    } deriving (Show, Generic)


makeClassy ''LdResult

instance Binary LdResult
instance FromJSON LdResult
instance ToJSON LdResult


-------------------------------------------------------------------------------
-- Helper Instances
-------------------------------------------------------------------------------

instance Default LdResult where
  def = LdResult
        { _ldFiles = Map.empty
        }


instance Monoid LdResult where
  mempty = def

  mappend r1 r2 = LdResult { _ldFiles = _ldFiles r1 <> _ldFiles r2
                           }

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

instance PP.Pretty LdResult where
  pretty r =
    PP.pretty $ Map.toList (r^.ldFiles)


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

singleton :: FilePath -> Text -> LdResult
singleton fp src =
  LdResult
  { _ldFiles = Map.singleton fp src
  }


toList :: LdResult -> [(FilePath, Text)]
toList = Map.toList . _ldFiles
