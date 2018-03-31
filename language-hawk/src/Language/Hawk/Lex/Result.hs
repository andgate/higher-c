{-# LANGUAGE  DeriveGeneric, TemplateHaskell #-}
module Language.Hawk.Lex.Result where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Set as Set

{-
-----------------------------------------------------------------------
-- Lex Result
-----------------------------------------------------------------------

data LexResult
  = LexResult
    { _lxTokens :: Map FilePath [[Token]]
    } deriving (Show, Generic)


makeClassy ''LexResult

instance Binary LexResult
instance FromJSON LexResult
instance ToJSON LexResult



-----------------------------------------------------------------------
-- Helper Instances
-----------------------------------------------------------------------

instance Default LexResult where
  def = empty


instance Semigroup LexResult where
  (<>) r1 r2
    = LexResult
      { _lxTokens = _lxTokens r1 <<>> _lxTokens r2 }
    where
      (<<>>) = Map.unionWith (++)

instance Monoid LexResult where
  mempty = empty




-----------------------------------------------------------------------
-- Pretty
-----------------------------------------------------------------------

instance Pretty LexResult where
  pretty =
    pretty . Map.toList . _lxTokens


-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

empty :: LexResult
empty =
  LxResult
  { _lxTokens = Map.empty
  }


singleton :: FilePath -> [[Token]] -> LexResult
singleton n ts =
  LxResult
  { _lxTokens = Map.singleton n ts
  }


fromList :: [(FilePath, [[Token]])] -> LexResult
fromList ls =
  LxResult
  { _lxTokens = Map.fromList ls
  }


toList :: LexResult -> [(FilePath, [[Token]])]
toList = Map.toList . _lxTokens

-}