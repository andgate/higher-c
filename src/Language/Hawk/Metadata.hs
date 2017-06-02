{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances,
    FlexibleContexts, GADTs, MultiParamTypeClasses, OverloadedStrings,
    TypeFamilies, RankNTypes #-}
module Language.Hawk.Metadata where


import Data.Binary (encode)

import Conduit
import Control.Monad (sequence, void, when)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Data.Traversable
import Data.Tree
import Data.Vector (Vector)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Hawk.Compile.Monad
import Language.Hawk.Compile.Source (HawkSource)
import Language.Hawk.Metadata.CacheStatus
import Language.Hawk.Parse.Document (Document(..), InfoDoc)
import Language.Hawk.Report.Result


import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Map                         as Map
import qualified Data.Text                        as Text
import qualified Data.Vector                      as V
import qualified Language.Hawk.Compile.Source as Src
import qualified Language.Hawk.Metadata.Namespace as NS
import qualified Language.Hawk.Metadata.Schema as Db
import qualified Language.Hawk.Parse as P
import qualified Language.Hawk.Report.Error       as Err
import qualified Language.Hawk.Report.Info        as Info
import qualified Language.Hawk.Report.Warning     as Warn
import qualified Language.Hawk.Syntax.AliasDefinition as AD
import qualified Language.Hawk.Syntax.DataDefinition as DD
import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.ExpressionDeclaration as EDecl
import qualified Language.Hawk.Syntax.ExpressionDefinition as EDef
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Literal as L
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.OpInfo as OI
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeClassDefinition as TCD
import qualified Language.Hawk.Syntax.TypeDeclaration as TD


type BackendT m a = ReaderT SqlBackend m a


staleAll :: MonadIO m => BackendT m ()
staleAll = do
  -- Everything with a cacheStatus needs to go here
  updateWhere [] [Db.ModuleCacheStatus =. Stale]
  updateWhere [] [Db.ModuleFileCacheStatus =. Stale]
  updateWhere [] [Db.ModulePathCacheStatus =. Stale]




insertPackage :: MonadIO m => Package -> BackendT m Db.PackageId
insertPackage (Package n srcDir) = do
  mayPkg <- getBy $ Db.UniquePackage n srcDir
  case mayPkg of
    Nothing -> insert $ Db.Package n srcDir
    Just (Entity pid _) -> return pid


-- Insert a hawk source by recusively inserting the module
-- and then inserting the module file.
insertSource :: MonadIO m => Db.PackageId -> HawkSource -> BackendT m (Result ())
insertSource pid src = do
  -- Build a name forest from the module file paths
  let mp   = Src.splitModulePath src
      fp = Src.srcPath src
      clk = Src.srcTimestamp src
  mid <- insertModuleRecursively mp
  when (Src.isHkFile fp) $
    void $ insertModuleFile pid mid fp clk
  return $ pure ()


-- | Insert a module, ensuring it's ancestors exist and module path is established
insertModuleRecursively :: MonadIO m => [String] -> BackendT m  Db.ModuleId
insertModuleRecursively mp = do
  -- Insert modules
  let qmps = Src.qualifyModulePath mp
  mpids <- mapM insertModule qmps

  -- Connect module paths
  let zipAdj = zip <*> tail
      mEdges = zipAdj mpids
  mapM_ insertModulePath mEdges

  -- Return the target module's ID
  return (last mpids)


insertModule :: MonadIO m => (String, String) -> BackendT m Db.ModuleId
insertModule (n, qn) = do
  let (n', qn') = (pack n, pack qn)
  may_m <- getBy $ Db.UniqueModule qn'
  case may_m of
    Nothing -> insert $ Db.Module n' qn' Fresh

    Just (Entity mid _) -> do
        update mid [Db.ModuleCacheStatus =. Preserved]
        return mid

        

insertModulePath :: MonadIO m => (Db.ModuleId, Db.ModuleId) -> BackendT m Db.ModulePathId
insertModulePath (a, b) = do
  may_mp <- getBy $ Db.UniqueModulePath a b
  case may_mp of
    Nothing ->
        insert $ Db.ModulePath a b Fresh

    Just (Entity mpid _) -> do
        update mpid [Db.ModulePathCacheStatus =. Preserved]
        return mpid


insertModuleFile :: MonadIO m => Db.PackageId -> Db.ModuleId -> FilePath -> UTCTime -> BackendT m Db.ModuleFileId
insertModuleFile pid mid fp clk = do
  let fp' = pack fp
  may_mf <- getBy $ Db.UniqueModuleFile pid mid fp'
  case may_mf of
    Nothing -> 
        insert $ Db.ModuleFile pid mid fp' clk Fresh False

    Just (Entity mfid mf)
      | clk > Db.moduleFileTimestamp mf -> do
          update mfid [ Db.ModuleFileTimestamp =. clk
                      , Db.ModuleFileCacheStatus =. Fresh
                      , Db.ModuleFileIsBuilt =. False
                      ]
          return mfid

      | otherwise -> do
          update mfid [ Db.ModuleFileCacheStatus =. Preserved ]
          return mfid

        


insertItem :: MonadIO m => Db.ModuleId -> I.Source -> BackendT m ()
insertItem modId i =
  case i of
    I.DepDecl d -> void $ insertDepDecl modId d
    I.ExprDef ed -> void $ insertExprDef modId ed
    I.AliasDef ad -> void $ insertAliasDef modId ad
    I.DataDef dd -> void $ insertDataDef modId dd
    I.TypeClassDef tcd -> void $ insertTypeClassDef modId tcd


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