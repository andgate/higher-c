{-# LANGUAGE  TemplateHaskell
  #-}
module Language.Hawk.SubtermCheck.Environment where

import Control.Lens
import Data.Text (Text)
import Data.Map (Map)
import Data.Monoid
import Language.Hawk.Syntax.Term.Scoped
import Language.Hawk.Syntax.Subterm

import qualified Data.Map as Map


-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

-- Types and their known kind names
data Env = SubtermEnv { _subterms :: Map Text Subterm }
  deriving (Eq, Show)


makeClassy ''Env


empty :: Env
empty = SubtermEnv Map.empty


extend :: HasEnv e => e -> (Text, Subterm) -> e
extend e (key, value) =
  e & env . subterms %~ Map.insert key value


remove :: HasEnv e => e -> Text -> e
remove e key =
  e & env . subterms %~ Map.delete key


extends :: HasEnv e => e -> [(Text, Subterm)] -> e
extends e xs =
  e & env . subterms %~ Map.union (Map.fromList xs)


lookup :: HasEnv e => Text -> e -> Maybe Subterm
lookup key e =
  Map.lookup key $ e ^. env . subterms


merge :: HasEnv e => e -> e -> e
merge e1 e2 =
  e1 & env . subterms %~ Map.union (e2 ^. env . subterms)


mergeMany :: HasEnv e => [e] -> Env
mergeMany = foldr (merge . view env) empty


mergeSome :: HasEnv e => [e] -> e
mergeSome = foldr1 merge


singleton :: Text -> Subterm -> Env
singleton key val = SubtermEnv $ Map.singleton key val

keys :: HasEnv e => e -> [Text]
keys e =
  Map.keys (e ^. env . subterms)


fromList :: [(Text, Subterm)] -> Env
fromList = SubtermEnv . Map.fromList

toList :: HasEnv e => e -> [(Text, Subterm)]
toList = Map.toList . view (env . subterms)


instance Monoid Env where
  mempty = empty
  mappend = merge
