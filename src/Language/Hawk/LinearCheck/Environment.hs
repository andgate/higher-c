{-# LANGUAGE  TemplateHaskell
  #-}
module Language.Hawk.LinearCheck.Environment where

import Control.Lens
import Data.Text (Text)
import Data.Map (Map)
import Data.Monoid
import Language.Hawk.Syntax.Type

import qualified Data.Map as Map


-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

data Env = TypeEnv { _types :: Map Text Scheme }
  deriving (Eq, Show)


makeClassy ''Env


empty :: Env
empty = TypeEnv Map.empty


extend :: HasEnv e => e -> (Text, Scheme) -> e
extend e (key, value) =
  e & env . types %~ Map.insert key value


remove :: HasEnv e => e -> Text -> e
remove e key =
  e & env . types %~ Map.delete key


extends :: HasEnv e => e -> [(Text, Scheme)] -> e
extends e xs =
  e & env . types %~ Map.union (Map.fromList xs)


lookup :: HasEnv e => Text -> e -> Maybe Scheme
lookup key e =
  Map.lookup key $ e ^. env . types


merge :: HasEnv e => e -> e -> e
merge e1 e2 =
  e1 & env . types %~ Map.union (e2 ^. env . types)


mergeMany :: HasEnv e => [e] -> Env
mergeMany = foldr (merge . view env) empty


mergeSome :: HasEnv e => [e] -> e
mergeSome = foldr1 merge


singleton :: Text -> Scheme -> Env
singleton key val = TypeEnv $ Map.singleton key val

keys :: HasEnv e => e -> [Text]
keys e =
  Map.keys (e ^. env . types)


fromList :: [(Text, Scheme)] -> Env
fromList = TypeEnv . Map.fromList

toList :: HasEnv e => e -> [(Text, Scheme)]
toList = Map.toList . view (env . types)


instance Monoid Env where
  mempty = empty
  mappend = merge
