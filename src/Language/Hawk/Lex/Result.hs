{-# LANGUAGE  DeriveGeneric, TemplateHaskell #-}
module Language.Hawk.Lex.Result where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Default.Class
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Text.PrettyPrint.Leijen.Text as PP


-----------------------------------------------------------------------
-- Lex Result
-----------------------------------------------------------------------

data LxResult
  = LxResult
    { _lxTokens :: Map FilePath [[Token]]
    } deriving (Show, Generic)


makeClassy ''LxResult

instance Binary LxResult
instance FromJSON LxResult
instance ToJSON LxResult



-----------------------------------------------------------------------
-- Helper Instances
-----------------------------------------------------------------------

instance Default LxResult where
  def = empty


instance Monoid LxResult where
  mempty = empty

  mappend r1 r2
    = LxResult
      { _lxTokens = _lxTokens r1 <<>> _lxTokens r2 }
    where
      (<<>>) = Map.unionWith (++)



-----------------------------------------------------------------------
-- Pretty
-----------------------------------------------------------------------

instance PP.Pretty LxResult where
  pretty =
    PP.pretty . Map.toList . _lxTokens


-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

empty :: LxResult
empty =
  LxResult
  { _lxTokens = Map.empty
  }


singleton :: FilePath -> [[Token]] -> LxResult
singleton n ts =
  LxResult
  { _lxTokens = Map.singleton n ts
  }


fromList :: [(FilePath, [[Token]])] -> LxResult
fromList ls =
  LxResult
  { _lxTokens = Map.fromList ls
  }


toList :: LxResult -> [(FilePath, [[Token]])]
toList = Map.toList . _lxTokens
