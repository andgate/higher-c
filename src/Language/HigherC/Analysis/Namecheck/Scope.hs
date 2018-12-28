{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.HigherC.Analysis.Namecheck.Scope where

import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Trie (Trie)
import Language.HigherC.Syntax.Concrete

import qualified Data.Text as T
import qualified Data.Trie as Trie
import qualified Data.HashMap.Strict as HMap

type Table a = HashMap Text a
type ModuleTable = Table Module
type InterfaceTable = Table Interface

-- -----------------------------------------------------------------------------
-- | Global Scope

data GlobalScope
  = GlobalScope
    { _globalScope              :: Scope
    , _globalModuleTable        :: ModuleTable
    , _globalInterfaceTable     :: InterfaceTable
    }

instance Semigroup GlobalScope where
  (<>) (GlobalScope a1 b1 c1)
       (GlobalScope a2 b2 c2)
    = GlobalScope (a1<>a2) (b1<>b2) (c1<>c2)

instance Monoid GlobalScope where
  mempty = GlobalScope mempty mempty mempty

-- -----------------------------------------------------------------------------
-- | Local Scope

data LocalScope
  = LocalScope
    { _localStack :: [Scope] }

instance Semigroup LocalScope where
  (<>) (LocalScope a1)
       (LocalScope a2)
    = LocalScope (a1<>a2)

instance Monoid LocalScope where
  mempty = LocalScope mempty


--searchLocalScope :: Text -> LocalScope -> [Text]
--searchLocalScope txt (LocalScope stack) = 

-- searchScopeVars

-- -----------------------------------------------------------------------------
-- | Scope

data Scope
  = Scope
    { _scopeVarNames           :: Trie
    , _scopeConNames           :: Trie
    , _scopeTypeNames          :: Trie
    }



instance Semigroup Scope where
  (<>) (Scope a1 b1 c1)
       (Scope a2 b2 c2)
    = Scope (a1<>a2) (b1<>b2) (c1<>c2)

instance Monoid Scope where
  mempty = Scope mempty mempty mempty

-- -----------------------------------------------------------------------------
-- | Lenses

makeLenses ''Scope
makeLenses ''LocalScope
makeLenses ''GlobalScope


-- -----------------------------------------------------------------------------
-- | Table Construction
 
buildInterfaceTable :: [IObject] -> InterfaceTable
buildInterfaceTable iobjs
  = HMap.fromList [ (unpackInterfacePath i, i) | i <- findInterfaces iobjs ]

buildModuleTable :: [Object] -> ModuleTable 
buildModuleTable objs
  = HMap.fromList [ (unpackModulePath m, m) | m <- concatMap findModules objs]


-- -----------------------------------------------------------------------------
-- | Scope Extraction

objectScope :: Object -> GlobalScope
objectScope obj = undefined

moduleScope :: Module -> GlobalScope
moduleScope m = undefined

iobjectScope :: IObject -> GlobalScope
iobjectScope obj = undefined

interfaceScope :: Interface -> GlobalScope
interfaceScope iface = undefined

statementScope :: Stmt -> LocalScope
statementScope stmt = undefined