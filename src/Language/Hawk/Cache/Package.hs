module Language.Hawk.Cache.Package where


import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Database.Persist
import Language.Hawk.Cache.Janitor
import Language.Hawk.Cache.Model
import Language.Hawk.Compile.State

import qualified Data.Text as T
import qualified Language.Hawk.Compile.Package as C


insertPackage :: ( MonadIO m
                 , MonadReader c m, C.HasPackage c
                 , MonadState s m, HasHkcState s
                 ) => BackendT m ()
insertPackage = do
  pkg <- lift $ view C.package
  let n = pkg ^. C.pkgName
      srcDir = T.pack $ pkg ^. C.pkgSrcDir
  
  mayPkg <- getBy $ UniquePackage n srcDir
  pid <-  case mayPkg of
            Nothing ->
              insert $ Package n srcDir
            
            Just (Entity pid _) -> do
              stalePkg pid
              return pid

  lift $ hkcPkgId .= pid