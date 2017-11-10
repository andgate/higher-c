{-# LANGUAGE  TemplateHaskell
  #-}
module Language.Hawk.KindsCheck.Environment where

import Control.Lens
import Data.Text (Text)
import Data.Map (Map)
import Data.Monoid
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.Kind

import qualified Data.Map as Map


-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

-- Types and their known kind names
data Env = KindEnv { _kinds :: Map Text Kind }
  deriving (Eq, Show)


makeClassy ''Env


empty :: Env
empty = KindEnv Map.empty


extend :: HasEnv e => e -> (Text, Kind) -> e
extend e (key, value) =
  e & env . kinds %~ Map.insert key value


remove :: HasEnv e => e -> Text -> e
remove e key =
  e & env . kinds %~ Map.delete key


extends :: HasEnv e => e -> [(Text, Kind)] -> e
extends e xs =
  e & env . kinds %~ Map.union (Map.fromList xs)


lookup :: HasEnv e => Text -> e -> Maybe Kind
lookup key e =
  Map.lookup key $ e ^. env . kinds


merge :: HasEnv e => e -> e -> e
merge e1 e2 =
  e1 & env . kinds %~ Map.union (e2 ^. env . kinds)


mergeMany :: HasEnv e => [e] -> Env
mergeMany = foldr (merge . view env) empty


mergeSome :: HasEnv e => [e] -> e
mergeSome = foldr1 merge


singleton :: Text -> Kind -> Env
singleton key val = KindEnv $ Map.singleton key val

keys :: HasEnv e => e -> [Text]
keys e =
  Map.keys (e ^. env . kinds)


fromList :: [(Text, Kind)] -> Env
fromList = KindEnv . Map.fromList

toList :: HasEnv e => e -> [(Text, Kind)]
toList = Map.toList . view (env . kinds)


instance Monoid Env where
  mempty = empty
  mappend = merge
