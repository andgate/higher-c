module Language.Hawk.Analysis.SymbolTable where

import qualified Data.Map as Map

data SymbolTable
  = SymbolTable 
  { symtab_scopes :: [Scope]
  , symtab_curr_path :: String
  , 
  }
  
data Scope
  = Scope [String]
  
  
symInsert
symLookup

enterScope
exitScope
scopeInsert

type StringMap a = Map.Map String a

-- A prefix trie is used to store the path of a hawk item by prefix.
-- This structure is ideal for collecting item names top-down.
-- Each prefix trie stores the current part of the path,
-- and the next possible paths that part leads too.
-- A leaf represents an item 
data PrefixTrie a
  = PrefixTrie String (StringMap (PrefixTrie a))
  | PrefixTrieLeaf a

-- A suffix trie is used to look up the a hawk name from a given suffix.
-- Insertion is slow with this, so it is not ideal for collecting symbols.
-- 
data SuffixTrie a
  = SuffixTrie (StringMap SuffixTrie)
  | SuffixTrieLeaf a