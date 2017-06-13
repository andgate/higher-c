module Language.Hawk.Cache.Package where


import Control.Monad.IO.Class (MonadIO)
import Database.Persist
import Language.Hawk.Cache.Janitor
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