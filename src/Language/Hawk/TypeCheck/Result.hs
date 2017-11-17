{-# LANGUAGE TemplateHaskell, DeriveGeneric, OverloadedStrings #-}
module Language.Hawk.TypeCheck.Result where

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
-- Type Check Result
-----------------------------------------------------------------------
 
data TcResult
  = TcResult
    { _tcSigs :: Map Text Scheme
    , _tcDecls :: Map Text [Exp]
    } deriving (Show, Generic)


makeClassy ''TcResult

instance Binary TcResult
instance FromJSON TcResult
instance ToJSON TcResult


-----------------------------------------------------------------------
-- Helper Instances
-----------------------------------------------------------------------

instance Default TcResult where
  def = TcResult
        { _tcSigs = Map.empty
        , _tcDecls = Map.empty
        }

          
instance Monoid TcResult where
  mempty = def

  mappend r1 r2
    = TcResult
      { _tcSigs = _tcSigs r1 <> _tcSigs r2
      , _tcDecls = _tcDecls r1 <<>> _tcDecls r2
      }
    where
      (<<>>) = Map.unionWith (++)


-----------------------------------------------------------------------
-- Pretty
-----------------------------------------------------------------------

instance PP.Pretty TcResult where
  pretty r =
    PP.textStrict "Signatures"
      PP.<$> PP.pretty (Map.toList $ _tcSigs r)
      PP.<$> PP.textStrict "Declarations"
      PP.<$> PP.pretty (Map.toList $ _tcDecls r)  


      
-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

singleton :: Text -> Scheme -> [Exp] -> TcResult
singleton n t es =
  TcResult { _tcSigs = Map.singleton n t
           , _tcDecls = Map.singleton n es
           }
