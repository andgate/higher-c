module Language.Hawk.Cache.Module where

import Control.Lens
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Chronicle
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Database.Persist
import Language.Hawk.Cache.Model
import Language.Hawk.Cache.Types
import Language.Hawk.Compile.Source (HawkSource)
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Language.Hawk.Compile.Package
import Language.Hawk.Compile.State

import qualified Language.Hawk.Compile.Source     as Src

-- Insert a hawk source by recusively inserting the module
-- and then inserting the module file.
insertSource :: ( MonadIO m
                , MonadReader c m, HasPackage c
                , MonadState s m, HasHkcState s
                )
             => HawkSource -> BackendT m ()
insertSource src = do
  -- Build a name forest from the module file paths
  let fp  = Src.srcPath src
      clk = Src.srcTimestamp src
  mp <- lift $ Src.splitModulePath src
  mid <- insertModuleRecursively mp
  when (Src.isHkFile fp) $
    void $ insertModuleFile mid fp clk


-- | Insert a module, ensuring it's ancestors exist and module path is established
insertModuleRecursively
    ::  ( MonadIO m
        , MonadReader c m, HasPackage c
        , MonadState s m, HasHkcState s
        )
    => [String] -> BackendT m  ModuleId
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


insertModule :: ( MonadIO m
                , MonadReader c m, HasPackage c
                , MonadState s m, HasHkcState s
                )
             => (String, String) -> BackendT m ModuleId
insertModule (n, qn) = do
  let (n', qn') = (pack n, pack qn)
  pid <- use hkcPkgId
  may_m <- getBy $ UniqueModule pid qn'
  case may_m of
    Nothing -> insert $ Module pid n' qn' Fresh

    Just (Entity mid _) -> do
        update mid [ModuleCacheStatus =. Preserved]
        return mid

        

insertModulePath :: ( MonadIO m
                    , MonadReader c m, HasPackage c
                    , MonadState s m, HasHkcState s
                    )
                 =>  (ModuleId, ModuleId) -> BackendT m ModulePathId
insertModulePath (a, b) = do
  pid <- lift $ use hkcPkgId
  may_mp <- getBy $ UniqueModulePath pid a b
  case may_mp of
    Nothing ->
        insert $ ModulePath pid a b Fresh

    Just (Entity mpid _) -> do
        update mpid [ModulePathCacheStatus =. Preserved]
        return mpid


insertModuleFile :: ( MonadIO m
                    , MonadReader c m, HasPackage c
                    , MonadState s m, HasHkcState s
                    )
                 =>  ModuleId -> FilePath -> UTCTime -> BackendT m ModuleFileId
insertModuleFile mid fp clk = do
  let fp' = pack fp
  pid <- lift $ use hkcPkgId
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
          updateWhere [ ItemMfid ==. mfid ] [ItemCacheStatus =. Preserved]
          return mfid