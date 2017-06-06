module Language.Hawk.Cache.Module where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Database.Persist
import Language.Hawk.Cache.Model
import Language.Hawk.Cache.Types
import Language.Hawk.Compile.Source (HawkSource)
import Language.Hawk.Report.Result

import qualified Language.Hawk.Compile.Source     as Src

-- Insert a hawk source by recusively inserting the module
-- and then inserting the module file.
insertSource :: MonadIO m => PackageId -> HawkSource -> BackendT m (Result ())
insertSource pid src = do
  -- Build a name forest from the module file paths
  let mp   = Src.splitModulePath src
      fp = Src.srcPath src
      clk = Src.srcTimestamp src
  mid <- insertModuleRecursively pid mp
  when (Src.isHkFile fp) $
    void $ insertModuleFile pid mid fp clk
  return $ pure ()


-- | Insert a module, ensuring it's ancestors exist and module path is established
insertModuleRecursively :: MonadIO m => PackageId -> [String] -> BackendT m  ModuleId
insertModuleRecursively pid mp = do
  -- Insert modules
  let qmps = Src.qualifyModulePath mp
  mpids <- mapM (insertModule pid) qmps

  -- Connect module paths
  let zipAdj = zip <*> tail
      mEdges = zipAdj mpids
  mapM_ (insertModulePath pid) mEdges

  -- Return the target module's ID
  return (last mpids)


insertModule :: MonadIO m => PackageId -> (String, String) -> BackendT m ModuleId
insertModule pid (n, qn) = do
  let (n', qn') = (pack n, pack qn)
  may_m <- getBy $ UniqueModule pid qn'
  case may_m of
    Nothing -> insert $ Module pid n' qn' Fresh

    Just (Entity mid _) -> do
        update mid [ModuleCacheStatus =. Preserved]
        return mid

        

insertModulePath :: MonadIO m => PackageId -> (ModuleId, ModuleId) -> BackendT m ModulePathId
insertModulePath pid (a, b) = do
  may_mp <- getBy $ UniqueModulePath pid a b
  case may_mp of
    Nothing ->
        insert $ ModulePath pid a b Fresh

    Just (Entity mpid _) -> do
        update mpid [ModulePathCacheStatus =. Preserved]
        return mpid


insertModuleFile :: MonadIO m => PackageId -> ModuleId -> FilePath -> UTCTime -> BackendT m ModuleFileId
insertModuleFile pid mid fp clk = do
  let fp' = pack fp
  may_mf <- getBy $ UniqueModuleFile pid mid fp'
  case may_mf of
    Nothing -> 
        insert $ ModuleFile pid mid fp' clk Fresh False

    Just (Entity mfid mf)
      | clk > moduleFileTimestamp mf -> do
          update mfid [ ModuleFileTimestamp =. clk
                      , ModuleFileCacheStatus =. Fresh
                      , ModuleFileIsBuilt =. False
                      ]
          return mfid

      | otherwise -> do
          update mfid [ ModuleFileCacheStatus =. Preserved ]
          return mfid