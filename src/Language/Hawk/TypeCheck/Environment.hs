{-# LANGUAGE  TemplateHaskell
  #-}
module Language.Hawk.TypeCheck.Environment where

import Control.Lens
import Data.Text (Text)
import Data.Map (Map)
import Data.Monoid
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.Signature

import qualified Data.Map as Map


-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

data Env = TypeEnv { _types :: Map Text Type }
  deriving (Eq, Show)


makeClassy ''Env


instance Monoid Env where
  mempty = empty
  mappend = merge
  

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

empty :: Env
empty = TypeEnv Map.empty


extend :: HasEnv e => e -> (Text, Type) -> e
extend e (key, value) =
  e & env . types %~ Map.insert key value


remove :: HasEnv e => e -> Text -> e
remove e key =
  e & env . types %~ Map.delete key


extends :: HasEnv e => e -> [(Text, Type)] -> e
extends e xs =
  e & env . types %~ Map.union (Map.fromList xs)


lookup :: HasEnv e => Text -> e -> Maybe Type
lookup key e =
  Map.lookup key $ e ^. env . types


merge :: HasEnv e => e -> e -> e
merge e1 e2 =
  e1 & env . types %~ Map.union (e2 ^. env . types)


mergeMany :: HasEnv e => [e] -> Env
mergeMany = foldr (merge . view env) empty


mergeSome :: HasEnv e => [e] -> e
mergeSome = foldr1 merge


singleton :: Text -> Type -> Env
singleton key val = TypeEnv $ Map.singleton key val

keys :: HasEnv e => e -> [Text]
keys e =
  Map.keys (e ^. env . types)


fromList :: [(Text, Type)] -> Env
fromList = TypeEnv . Map.fromList


toList :: HasEnv e => e -> [(Text, Type)]
toList = Map.toList . view (env . types)


fromSig :: Sig -> Env
fromSig (Sig n t) = singleton n t

fromSigs :: [Sig] -> Env
fromSigs sigs = fromList $ zip ns ts
  where
    ns = map _sigName sigs
    ts = map _sigType sigs


fromMap :: Map Text Type -> Env
fromMap ts =
  TypeEnv { _types = ts }

toMap :: HasEnv e => e -> Map Text Type
toMap = view (env .types)
