module Data.Trie where

import Data.Map.Lazy (Map)
import Data.Maybe
import Data.Monoid
import Data.Semigroup

import qualified Data.Map.Lazy as M


data Trie = Trie { trieChildren :: Map Char Trie }

instance Semigroup Trie where
  (<>) = merge

instance Monoid Trie where
  mempty = empty

empty :: Trie
empty = Trie M.empty


insert :: String -> Trie -> Trie
insert [] trie = trie
insert (c:cs) (Trie m)
  = Trie m'
  where
    m' = M.alter (Just . insert cs . fromMaybe empty) c m


member :: String -> Trie -> Bool
member = member' True
  where
    member' :: Bool -> String -> Trie -> Bool
    member' False _  _ = False
    member' p     [] _ = p
    member' p (c:cs) (Trie m)
      = member' (p && hasChild) cs child
      where
        maybe_child = M.lookup c m
        hasChild = isJust maybe_child
        child = fromMaybe empty maybe_child


toList :: Trie -> [String]
toList (Trie cs)
  = mconcat [ map (c:) (toList t) | (c, t) <- M.toList cs]

merge :: Trie -> Trie -> Trie
merge (Trie m1) (Trie m2)
  = Trie $ M.unionWith merge m1 m2