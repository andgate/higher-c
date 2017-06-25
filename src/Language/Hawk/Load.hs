{-# LANGUAGE  FlexibleContexts
            , ScopedTypeVariables
            , RankNTypes
            , MultiWayIf
  #-}
module Language.Hawk.Load where

import Control.Lens
import Control.Exception
import Control.Monad.Except
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Control.Monad.Log
import Control.Monad.State
import Control.Monad.Reader
import Language.Hawk.Compile.Config
import Language.Hawk.Compile.State
import Language.Hawk.Load.Error
import Language.Hawk.Load.Message
import System.IO.Error

import qualified Data.Text.IO as T

load
  :: ( MonadState s m, HasHkcState s
     , MonadReader c m, HasHkcConfig c
     , MonadLog (WithSeverity (WithTimestamp msg)) m, AsLoadMsg msg
     , MonadChronicle [WithTimestamp e] m, AsLoadErr e
     , MonadIO m
     )
  => m ()
load = condemn $
  do
    fps <- view hkcSrcFiles
    forM_ fps $ \fp -> do
      srcOrExc <- liftIO . try . T.readFile $ fp
      case srcOrExc of
          Left ex ->
            discloseNow $ mkLoadErr fp ex
            
          Right src -> do
            logInfo =<< timestamp (_FileFound # fp)
            hkcSrcs %= (src:)


mkLoadErr :: (AsLoadErr e) => FilePath -> IOException -> e
mkLoadErr fp ex
  | isDoesNotExistError ex =
      _FileNotFound # fp

  | isAlreadyInUseError ex =
      _FileInUse # fp

  | isPermissionError ex   =
      _PermissionDenied # fp

  | otherwise              =
      _UndefinedLoadErr # fp