module Language.Hawk.Cache.Package where


import Control.Monad.IO.Class (MonadIO)
import Database.Persist
import Language.Hawk.Cache.Model
import Language.Hawk.Cache.Types
import qualified Language.Hawk.Compile.Monad as C



insertPackage :: MonadIO m => C.Package -> BackendT m PackageId
insertPackage (C.Package n srcDir) = do
  mayPkg <- getBy $ UniquePackage n srcDir
  case mayPkg of
    Nothing -> insert $ Package n srcDir
    Just (Entity pid _) -> do
      stalePkg pid
      return pid


stalePkg :: MonadIO m => PackageId -> BackendT m ()
stalePkg pid = do
  -- Everything with a cacheStatus needs to go here
  updateWhere [ModulePkg ==. pid] [ModuleCacheStatus =. Stale]
  updateWhere [ModuleFilePkg ==. pid] [ModuleFileCacheStatus =. Stale]
  updateWhere [ModulePathPkg ==. pid] [ModulePathCacheStatus =. Stale]