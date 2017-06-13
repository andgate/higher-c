module Language.Hawk.Cache.Janitor where

import Control.Monad.IO.Class (MonadIO)
import Database.Persist
import Language.Hawk.Cache.Model
import Language.Hawk.Cache.Types


stalePkg :: MonadIO m => PackageId -> BackendT m ()
stalePkg pid = do
  -- Everything with a cacheStatus needs to go here
  updateWhere [ModulePkg ==. pid]     [ModuleCacheStatus =. Stale]
  updateWhere [ModuleFilePkg ==. pid] [ModuleFileCacheStatus =. Stale]
  updateWhere [ModulePathPkg ==. pid] [ModulePathCacheStatus =. Stale]
  updateWhere [ItemPkg ==. pid]       [ItemCacheStatus =. Stale]
  

removeStale :: MonadIO m => BackendT m ()
removeStale = do
  deleteWhere  [ModuleCacheStatus ==. Stale]
  deleteWhere  [ModuleFileCacheStatus ==. Stale]
  deleteWhere  [ModulePathCacheStatus ==. Stale]
  deleteCascadeWhere  [ItemCacheStatus ==. Stale]