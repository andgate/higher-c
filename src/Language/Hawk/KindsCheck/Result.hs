{-# LANGUAGE TemplateHaskell, DeriveGeneric, OverloadedStrings #-}
module Language.Hawk.KindsCheck.Result where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Default.Class
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax

import qualified Data.Map.Strict as Map
import qualified Text.PrettyPrint.Leijen.Text as PP


-----------------------------------------------------------------------
-- Kinds Check Result
-----------------------------------------------------------------------

data KcResult
  = KcResult
    { _kcSigs :: Map Text Scheme
    , _kcDecls :: Map Text [Exp]
    } deriving (Show, Generic)


makeClassy ''KcResult

instance Binary KcResult
instance FromJSON KcResult
instance ToJSON KcResult


-----------------------------------------------------------------------
-- Helper Instances
-----------------------------------------------------------------------

instance Default KcResult where
  def = KcResult
        { _kcSigs = Map.empty
        , _kcDecls = Map.empty
        }

          
instance Monoid KcResult where
  mempty = def

  mappend r1 r2
    = KcResult
      { _kcSigs = _kcSigs r1 <> _kcSigs r2
      , _kcDecls = _kcDecls r1 <> _kcDecls r2
      }


-----------------------------------------------------------------------
-- Pretty
-----------------------------------------------------------------------

instance PP.Pretty KcResult where
  pretty r =
    PP.textStrict "Signatures"
      PP.<$> PP.pretty (Map.toList $ _kcSigs r)
      PP.<$> PP.textStrict "Declarations"
      PP.<$> PP.pretty (Map.toList $ _kcDecls r)  


-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

singleton :: Text -> Scheme -> [Exp] -> KcResult
singleton n t es =
  KcResult { _kcSigs = Map.singleton n t
           , _kcDecls = Map.singleton n es
           }
