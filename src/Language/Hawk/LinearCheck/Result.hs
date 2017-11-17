{-# LANGUAGE TemplateHaskell, DeriveGeneric, OverloadedStrings #-}
module Language.Hawk.LinearCheck.Result where

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
-- Linear Check Result
-----------------------------------------------------------------------

data LcResult
  = LcResult
    { _lcSigs :: Map Text Scheme
    , _lcDecls :: Map Text [Exp]
    } deriving (Show, Generic)


makeClassy ''LcResult

instance Binary LcResult
instance FromJSON LcResult
instance ToJSON LcResult


-----------------------------------------------------------------------
-- Helper Instances
-----------------------------------------------------------------------

instance Default LcResult where
  def = LcResult
        { _lcSigs = Map.empty
        , _lcDecls = Map.empty
        }

          
instance Monoid LcResult where
  mempty = def

  mappend r1 r2
    = LcResult
      { _lcSigs = _lcSigs r1 <> _lcSigs r2
      , _lcDecls = _lcDecls r1 <> _lcDecls r2
      }

-----------------------------------------------------------------------
-- Pretty
-----------------------------------------------------------------------

instance PP.Pretty LcResult where
  pretty r =
    PP.textStrict "Signatures"
      PP.<$> PP.pretty (Map.toList $ _lcSigs r)
      PP.<$> PP.textStrict "Declarations"
      PP.<$> PP.pretty (Map.toList $ _lcDecls r)


-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

singleton :: Text -> Scheme -> [Exp] -> LcResult
singleton n t es =
  LcResult { _lcSigs = Map.singleton n t
           , _lcDecls = Map.singleton n es
           }
