{-# LANGUAGE  DeriveGeneric, TemplateHaskell #-}
module Language.Hawk.Parse.Result where

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
-- Parse Result
-----------------------------------------------------------------------

data PsResult
  = PsResult
    { _psNames :: Set Text
    , _psSigs :: Map Text Type
    , _psDecls :: Map Text [Exp]
    } deriving (Show, Generic)


makeClassy ''PsResult

instance Binary PsResult
instance FromJSON PsResult
instance ToJSON PsResult


-----------------------------------------------------------------------
-- Helper Instances
-----------------------------------------------------------------------

instance Default PsResult where
  def = empty


instance Monoid PsResult where
  mempty = empty
  mappend = merge


-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

empty :: PsResult
empty =
  PsResult
    { _psNames = Set.empty
    , _psSigs = Map.empty
    , _psDecls = Map.empty
    }


singleton :: Text -> Maybe Type -> Maybe Exp -> PsResult
singleton n may_t may_e =
  PsResult
    { _psNames = Set.singleton n
    , _psSigs = case may_t of
                  Just t -> Map.singleton n t
                  Nothing -> Map.empty
    , _psDecls = case may_e of
                   Just e -> Map.singleton n [e]
                   Nothing -> Map.empty
    }


merge :: PsResult -> PsResult -> PsResult
merge r1 r2 =
  PsResult { _psNames = _psNames r1 <> _psNames r2
           , _psSigs  = _psSigs  r1 <> _psSigs  r2
           , _psDecls = _psDecls r1 <<>> _psDecls r2
           }
  where
    (<<>>) = Map.unionWith (++)
  
