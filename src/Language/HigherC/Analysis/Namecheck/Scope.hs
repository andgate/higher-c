{-# LANGUAGE LambdaCase #-}
module Language.HigherC.Analysis.Namecheck.Scope where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Trie (Trie)
import Language.HigherC.Syntax.Concrete

import qualified Data.Text as T
import qualified Data.Trie as Trie
import qualified Data.HashMap.Strict as HMap

type InterfaceMap = HashMap Text Interface

data Scope
  = Scope
    { scopeNames          :: Trie
    , scopeIfaces         :: InterfaceMap
    }

instance Semigroup Scope where
  (<>) (Scope a1 b1)
       (Scope a2 b2)
    = Scope (a1<>a2) (b1<>b2)

instance Monoid Scope where
  mempty = Scope mempty mempty

 
objectScope :: Object -> Scope
objectScope obj = undefined

moduleScope :: Module -> Scope
moduleScope m = undefined

iobjectScope :: IObject -> Scope
iobjectScope obj = undefined

interfaceScope :: Interface -> Scope
interfaceScope iface = undefined