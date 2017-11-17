{-# LANGUAGE  DeriveGeneric, TemplateHaskell #-}
module Language.Hawk.NameCheck.Result where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Default.Class
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Set as Set


-----------------------------------------------------------------------
-- Name Check Result
-----------------------------------------------------------------------

data NcResult
  = NcResult
    { _ncNames :: Set Text
    , _ncSigs :: Map Text Type
    , _ncDecls :: Map Text [Exp]
    } deriving (Show, Generic)


makeClassy ''NcResult

instance Binary NcResult
instance FromJSON NcResult
instance ToJSON NcResult


instance Default NcResult where
  def = NcResult
        { _ncNames = Set.empty
        , _ncSigs = Map.empty
        , _ncDecls = Map.empty
        }


instance Monoid NcResult where
  mempty = def

  mappend r1 r2 = NcResult { _ncNames = _ncNames r1 <> _ncNames r2
                           , _ncSigs  = _ncSigs r1 <> _ncSigs r2
                           , _ncDecls = _ncDecls r1 <> _ncDecls r2
                           }


singleton :: Text -> Maybe Type -> Maybe Exp -> NcResult
singleton n may_t may_e =
  NcResult
  { _ncNames = Set.singleton n
  , _ncSigs = case may_t of
                Just t -> Map.singleton n t
                Nothing -> Map.empty
  , _ncDecls = case may_e of
                 Just e -> Map.singleton n [e]
                 Nothing -> Map.empty
  }
