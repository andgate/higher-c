{-# LANGUAGE  ScopedTypeVariables,
              TypeSynonymInstances,
              EmptyDataDecls, 
              FlexibleContexts, 
              GADTs, 
              GeneralizedNewtypeDeriving, 
              MultiParamTypeClasses, 
              OverloadedStrings, 
              QuasiQuotes, 
              TemplateHaskell,
              TypeFamilies,
              RankNTypes
#-}
module Language.Hawk.Metadata where


import Data.Binary (encode, decode)

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Traversable
import Data.Tree
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Hawk.Compile.Monad
import System.FilePath ( (</>), (<.>), takeExtension, takeBaseName, splitDirectories )


import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Map                         as Map
import qualified Data.Text                        as Text
import qualified Language.Hawk.Metadata.Namespace as NS
import qualified Language.Hawk.Metadata.Schema as Db
import qualified Language.Hawk.Parse as P
import qualified Language.Hawk.Syntax.AliasDefinition as AD
import qualified Language.Hawk.Syntax.DataDefinition as DD
import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.ExpressionDeclaration as EDecl
import qualified Language.Hawk.Syntax.ExpressionDefinition as EDef
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Literal as L
import qualified Language.Hawk.Syntax.Module as M
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.OpInfo as OI
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeClassDefinition as TCD
import qualified Language.Hawk.Syntax.TypeDeclaration as TD


type BackendT m a = ReaderT SqlBackend m a


insertPackage :: MonadIO m => Package -> BackendT m Db.PackageId
insertPackage (Package n srcDir) = do
  mayPkg <- getBy $ Db.UniquePackage n srcDir
  case mayPkg of
    Nothing -> insert $ Db.Package n srcDir
    Just (Entity pid _) -> return pid



insertModules :: MonadIO m => Db.PackageId -> [FilePath] -> BackendT m [(FilePath, Db.ModuleId)]
insertModules pid fps = do
  -- Build a name forest from the module file paths
  let modlForest = toForest . map (qualifyModulePathParts . tail . splitBases) $ fps
  modlIdForest <- forestMapM insertModule modlForest

  -- Build module edge list paths from the previous id forest
  let modlIdPaths = fromForest modlIdForest
      modlEdges = moduleEdges modlIdPaths
      modlIds = map last modlIdPaths
      modlFpIds = zip fps modlIds 
  mapM_ insertModulePath modlEdges
  mapM_ insertModuleFilepath modlFpIds
  mapM_ (insertPackageModule pid) modlIds
  return modlFpIds

insertModule :: MonadIO m => (String, String) -> BackendT m Db.ModuleId
insertModule (n, qn) = do
  mayModl <- getBy $ Db.UniqueModule n' qn'
  case mayModl of
    Nothing -> insert $ Db.Module n' qn'
    Just (Entity mid _) -> return mid
  where
    n' = Text.pack n
    qn' = Text.pack qn


insertModulePath :: MonadIO m => (Db.ModuleId, Db.ModuleId) -> BackendT m Db.ModulePathId
insertModulePath (a, b) = do
  mayModlPath <- getBy $ Db.UniqueModulePath a b
  case mayModlPath of
    Nothing -> insert $ Db.ModulePath a b
    Just (Entity mpid _) -> return mpid

insertModuleFilepath :: MonadIO m => (FilePath, Db.ModuleId) -> BackendT m Db.ModuleFilepathId
insertModuleFilepath (path, modl) = do
  let path' = Text.pack path
  mayModlFile <- getBy $ Db.UniqueModuleFilepath modl path'
  case mayModlFile of
    Nothing -> insert $ Db.ModuleFilepath modl path'
    Just (Entity mfid _) -> return mfid


insertPackageModule :: MonadIO m => Db.PackageId -> Db.ModuleId -> BackendT m Db.PackageModuleId
insertPackageModule pid mid = do
  mayPkgMdl <- getBy $ Db.UniquePackageModule pid mid
  case mayPkgMdl of
    Nothing -> insert $ Db.PackageModule pid mid
    Just (Entity pmid _) -> return pmid

qualifyModulePathParts :: [String] -> [(String, String)]
qualifyModulePathParts ns = r
  where
    (_, r) = mapAccumL f [] ns
    f a b =
      let a' = a ++ [b]
      in (a', (b, intercalate "." a'))


moduleEdges :: [[Db.ModuleId]] -> [(Db.ModuleId, Db.ModuleId)]
moduleEdges = concatMap zipAdj
  where
    zipAdj = zip <*> tail
    

treeMapM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
treeMapM f (Node a ts) = do
  b <- f a
  ts' <- forestMapM f ts
  return $ Node b ts'

forestMapM :: Monad m => (a -> m b) -> Forest a -> m (Forest b)
forestMapM f ts = mapM (treeMapM f) ts


toForest :: (Ord a) => [[a]] -> Forest a
toForest r = unfoldForest (\(a, rs) -> (a, levelEntries rs))
                          (levelEntries r)
  where
    levelMap :: (Ord a) => [[a]] -> Map a [[a]]
    levelMap aa = Map.fromListWith (++) [ (a, [as]) | (a:as) <- aa ]

    levelEntries :: (Ord a) => [[a]] -> [(a, [[a]])]
    levelEntries = Map.toList . levelMap

fromForest :: Forest a -> [[a]]
fromForest [] = [[]]
fromForest f  = concat [ map (a:) (fromForest subf) | Node a subf <- f ]


splitBases :: FilePath -> [String]
splitBases = map takeBaseName . splitDirectories

{-
  where 
    insertModule' :: MonadIO m => [Db.ModuleId] -> [Text] -> BackendT m Db.ModuleId
    insertModule' [] [] = error "Cannot insert a module without a path."
    insertModule' (p:[]) [] = return p
    insertModule' (p:ps) [] = insertModule' ps []
    insertModule' parents (curr:rest) = do
      mayModl <- getBy $ Db.UniqueModule pkgId parents curr
      mid <- case mayModl of
                  Nothing -> insert $ Db.Module pkgId parents curr
                  Just (Entity mid _) -> return mid
      insertModule' (parents ++ [mid]) rest

insertModule :: MonadIO m => [FilePath] -> BackendT m [Db.ModuleId]
insertModule 
-}

        


insertItem :: MonadIO m => Db.ModuleId -> I.Source -> BackendT m ()
insertItem modId i =
  case i of
    I.DepDecl d -> insertDepDecl modId d >> return ()
    I.ExprDef ed -> insertExprDef modId ed >> return ()
    I.AliasDef ad -> insertAliasDef modId ad >> return ()
    I.DataDef dd -> insertDataDef modId dd >> return ()
    I.TypeClassDef tcd -> insertTypeClassDef modId tcd >> return ()


insertDepDecl :: MonadIO m => Db.ModuleId -> I.Dependency -> BackendT m Db.DependencyId
insertDepDecl modId (I.Dep q p a) = do
  let pdat = toStrict $ encode p
  insert $ Db.Dependency modId q pdat a


insertExprDef :: MonadIO m => Db.ModuleId -> EDef.Source -> BackendT m Db.ExprDefId
insertExprDef modId (EDef.ExprDef d e) = do
  declId <- insertExprDecl modId d
  let edat  = toStrict $ encode e
  insert $ Db.ExprDef modId declId edat

insertExprDecl :: MonadIO m => Db.ModuleId -> EDecl.Source -> BackendT m Db.ExprDeclId
insertExprDecl modId (EDecl.ExprDecl (N.Name n p) oi vs t) = do
  opId <- insertOperator modId n oi
  let tdat  = toStrict $ encode t
  insert $ Db.ExprDecl modId n opId tdat
  
insertTypeDecl :: MonadIO m => Db.ModuleId -> TD.Source -> BackendT m Db.TypeDeclId
insertTypeDecl modId (TD.TypeDecl ctx (N.Name n p) oi ts) = do
  opId <- insertOperator modId n oi 
  let ctxdat = toStrict $ encode ctx
      tsdat  = map (toStrict . encode) ts
  insert $ Db.TypeDecl modId n opId ctxdat tsdat
  
insertOperator :: MonadIO m => Db.ModuleId -> Text -> OI.OpInfo -> BackendT m Db.OpId
insertOperator modId n (OI.OpInfo p a) =
  insert $ Db.Op modId n (fromIntegral p) a

insertAliasDef :: MonadIO m => Db.ModuleId -> AD.Source -> BackendT m Db.AliasDefId  
insertAliasDef modId (AD.AliasDef d t) = do
  declId <- insertTypeDecl modId d
  let tdat  = toStrict $ encode t
  insert $ Db.AliasDef modId declId tdat

insertDataDef :: MonadIO m => Db.ModuleId -> DD.Source -> BackendT m Db.DataDefId
insertDataDef modId (DD.DataDef d b) = do
  declId <- insertTypeDecl modId d
  -- Create an entry for the data def
  ddId <- insert $ Db.DataDef modId declId []
  
  -- insert the constructors and update the data def entry
  consIds <- mapM (insertDataCons modId ddId) b
  update ddId [Db.DataDefBody =. consIds]
  return ddId

insertDataCons :: MonadIO m => Db.ModuleId -> Db.DataDefId -> DD.DCSource -> BackendT m Db.DataConId 
insertDataCons modId ddId (DD.DataCons (N.Name n p) t b) = do
  let tdat = toStrict $ encode t 
  conId <- insert $ Db.DataCon modId ddId n tdat []
  rowIds <- insertMany (mkDataRow modId ddId conId <$> b)
  update conId [Db.DataConRows =. rowIds]
  return conId

mkDataRow :: Db.ModuleId -> Db.DataDefId -> Db.DataConId -> DD.DRSource -> Db.DataRow 
mkDataRow modId ddId conId (DD.DataRow mn t) =
  let tdat = toStrict $ encode t
      mtxt = N.exLocal <$> mn
  in Db.DataRow modId ddId conId mtxt tdat
  
insertTypeClassDef :: MonadIO m => Db.ModuleId -> TCD.Source -> BackendT m Db.TypeClassDefId  
insertTypeClassDef modId (TCD.TypeClassDef d eds) = do
  decId <- insertTypeDecl modId d
  edIds <- mapM (insertExprDef modId) eds
  insert $ Db.TypeClassDef modId decId edIds