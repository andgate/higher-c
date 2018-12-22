{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Language.HigherC.Compile where

import Prelude hiding (lex)


import Control.Monad.Except
import Data.Foldable
import Data.Functor.Identity
import Data.Graph (Graph, Edge, Vertex)
import Data.HashMap.Strict (HashMap, (!))
import Data.IntMap.Strict (IntMap)
import Data.List hiding (concatMap)
import Data.Map.Strict (Map)
import Data.Tree (foldTree)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.IO as T

import Language.HigherC.Parse (parseObject)
import Language.HigherC.Parse.Error
import Language.HigherC.Lex (lex)
import Language.HigherC.Lex.Error

import qualified Language.HigherC.Syntax.Concrete as C

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)

import qualified Data.Graph          as G

import qualified Data.IntMap.Strict  as IMap
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import qualified Data.Tree           as Tree


data CompileError
  = ModuleNameCollision C.ModulePath [C.ModulePath]
  | ModuleNotFound C.Import


readObject :: FilePath -> IO C.Object
readObject objFp = do
  objText <- T.readFile objFp
  mtoks <- runExceptT $ do
    let lexResult = withExcept PLexErr (lex objFp objText)
    mapExceptT (return . runIdentity) lexResult

  case mtoks of
    Left err -> do
      putDoc $ pretty err
      error "Could not complete lexical analysis"

    Right toks -> do
      -- putDoc (vsep $ pretty <$> toks)
      let obj = (parseObject toks) { C.objFiles = [objFp] }
      -- putDoc $ pretty obj
      return obj


computeBuildOrder :: Graph -> [C.Object] -> [C.Object]
computeBuildOrder g objs
  = buildOrder
  where
    buildOrder = mconcat <$> componentObjects

    componentObjects :: [[C.Object]]
    componentObjects = do
      vs <- componentVerts
      return (lookupObj <$> vs)
    
    componentVerts :: [[Vertex]]
    componentVerts = Tree.flatten <$> G.scc g

    lookupObj:: Vertex -> C.Object
    lookupObj = (objectMap' IMap.!)

    objectMap' :: IntMap C.Object
    objectMap' = objectMap objs



buildObjectGraph :: [C.Object] -> Graph
buildObjectGraph objs
  = G.buildG bounds edges
  where
    dict = mapModules objs
    edges = findEdges dict objs
    bounds = (0, length objs - 1)


-- Check objects for errors
checkObjectGraph :: [C.Object] -> [CompileError]
checkObjectGraph objs
  = checkModuleNameCollision objs <> checkImports objs


-- Check to see if any two modules share the same name.
-- Module name collisions are forbidden in higher-c, for now.
checkModuleNameCollision :: [C.Object] -> [CompileError]
checkModuleNameCollision objs
  = errs
  where
    errs = uncurry ModuleNameCollision <$> collisions

    collisions :: [(C.ModulePath, [C.ModulePath])]
    collisions = filter hasConflicts (Map.toList collision_map)

    hasConflicts :: (C.ModulePath, [C.ModulePath]) -> Bool
    hasConflicts (_, conflicts) = not . null $ conflicts
    
    collision_map :: Map C.ModulePath [C.ModulePath]
    collision_map = foldr insert_path Map.empty all_module_paths

    all_module_paths :: [C.ModulePath]
    all_module_paths = concatMap C.findModuleDefnPaths objs

    insert_path :: C.ModulePath -> Map C.ModulePath [C.ModulePath] -> Map C.ModulePath [C.ModulePath]
    insert_path mp dict = 
      case Map.lookup mp dict of
        Nothing       -> Map.insert mp [] dict
        Just _        -> Map.adjust (\conflicts -> conflicts <> [mp]) mp dict
    

-- Check to see if any imports point to an undefined module
checkImports :: [C.Object] -> [CompileError]
checkImports objs
  = concatMap (checkImportsOf all_module_paths) objs
  where
    ex_path :: C.Object -> [Text]
    ex_path obj = C.unpackPath <$> C.findModuleDefnPaths obj
    all_module_paths = Set.fromList $ mconcat (ex_path <$> objs)

checkImportsOf :: Set Text -> C.Object -> [CompileError]
checkImportsOf dict obj
  = errs
    where
      import_paths = C.findModuleImportsRec obj
      invalid_paths = filter isMissing import_paths
      isMissing (C.Import _ mp) = C.unpackPath mp `Set.notMember` dict
      errs = ModuleNotFound <$> invalid_paths


-- | Map from vertices to objects
objectMap :: [C.Object] -> IntMap C.Object
objectMap = IMap.fromList . objectIndex

-- | Association list of objects and vertices
objectIndex :: [C.Object] -> [(Vertex, C.Object)]
objectIndex objs = zip [0..] objs

-- | Map modules names to object vertices
mapModules :: [C.Object] -> (HashMap Text Vertex)
mapModules objs
  = HMap.unions (mapModulesOf <$> objectIndex objs)

-- | Builds a map from module names in an object to the objects vertex 
mapModulesOf :: (Vertex, C.Object) -> HashMap Text Vertex
mapModulesOf (v, obj) =
  foldr insert HMap.empty module_paths
  where
    insert k = HMap.insert k v
    module_paths = C.unpackPath <$> C.findModuleDefnPaths obj

moduleDictOf :: [C.Object] -> HashMap Text C.Module
moduleDictOf objs =
  foldr insertModule HMap.empty all_modules
  where
    insertModule m = HMap.insert (C.unpackPath $ C.modName m) m

    all_modules :: [C.Module]
    all_modules = concatMap C.findModulesRec objs


-- | Find graph edges
findEdges :: HashMap Text Vertex -- Map from module names to their vertices
          -> [C.Object]  -- Vertex and Object associations
          -> [Edge]
findEdges dict objs
  = concatMap (findEdgesOf dict) (objectIndex objs)


findEdgesOf :: HashMap Text Vertex -- Map from module names to their vertices
            -> (Vertex, C.Object)  -- Vertex and Object association
            -> [Edge]
findEdgesOf dict (key, obj) = import_edges
  where
    import_paths = C.unpackImport <$> C.findModuleImportsRec obj
    import_verts = map (dict !) import_paths
    import_edges = map (key, ) import_verts
{-

data BuildEnv
  = BuildEnv
    { envMods    :: [C.Module]
    , envDecls   :: [C.Decl]
    , envFuncs   :: [C.FuncDecl]
    , envExterns :: [C.FuncExtern]

    , envCtors   :: [C.CtorDecl]
    , envDtors   :: [C.DtorDecl]

    , envTypeDefns :: [C.TypeDefn]
    , envAliases   :: [C.AliasDefn]

    , envClasses :: [C.ClassDefn]
    , envInsts  :: [C.InstDefn]


    , envVars :: HashMap Text (C.Name, C.Kind)
    , envFuncs :: HashMap Text [(C.Name, C.Type)]
    , envTypes :: HashMap Text (C.Name, C.Kind)
    , envCons :: HashMap Text (C.Name, C.Type)

    , envOpDecls :: [C.OpDecl]
    }

instance Semigroup GlobalScope where
  (<>) g1 g2
    = GlobalScope
      { objectFuncs   = objectFuncs   g1 <> objectFuncs   g2
      , externFuncs   = externFuncs   g1 <> externFuncs   g2
      , externTypes   = externTypes   g1 <> externTypes   g2
      , externMods    = externMods    g1 <> externMods    g2
      , operatorTable = operatorTable g1 <> operatorTable g2
      }

instance Monoid GlobalScope where
  mempty = GlobalScope
    { externFuncs   = mempty
    , externTypes   = mempty
    , externMods    = mempty
    , objectFuncs   = mempty
    , objectTypes   = mempty
    , objectMods    = mempty
    , operatorTable = mempty
    }


gatherImportEnv :: C.Object -> [C.Object] -> GlobalScope
gatherImportEnv obj objs
  = undefined
  where
    module_dict = modulesDictOf objs
    import_paths = C.unpackImport <$> C.objImportsRec obj
-}