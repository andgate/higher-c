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
import Control.Monad.Trans.Control
import Data.Bag
import Data.Text (Text)
import Data.Default.Instances.Text ()
import Language.Hawk.Compile.Config
import Language.Hawk.Compile.State
import Language.Hawk.Load.Error
import Language.Hawk.Load.Message
import System.IO.Error

import qualified Data.Text.IO as T

load
  :: ( MonadReader c m, HasHkcConfig c
     , MonadLog (WithSeverity (WithTimestamp msg)) m, AsLoadMsg msg
     , MonadChronicle (Bag (WithTimestamp e)) m, AsLoadErr e
     , MonadIO m, MonadBaseControl IO m
     )
  => m [(FilePath, Text)]
load =
  condemn $ do
    fps <- view hkcSrcFiles
    mapM loadFile fps


loadFile
    ::  ( MonadReader c m, HasHkcConfig c
        , MonadLog (WithSeverity (WithTimestamp msg)) m, AsLoadMsg msg
        , MonadChronicle (Bag (WithTimestamp e)) m, AsLoadErr e
        , MonadIO m
        )
  => FilePath -> m (FilePath, Text)
loadFile fp = do
  srcOrExc <- liftIO . try . T.readFile $ fp
  case srcOrExc of
      Left ex ->
        discloseNow $ mkLoadErr fp ex
        
      Right src -> do
        logInfo =<< timestamp (_FileFound # fp)
        return (fp, src)


mkLoadErr :: (AsLoadErr e) => FilePath -> IOException -> e
mkLoadErr fp ex
  | isDoesNotExistError ex =
      _FileNotFound # fp

  | isAlreadyInUseError ex =
      _FileInUse # fp

  | isPermissionError ex   =
      _PermissionDenied # fp

  | otherwise              =
      _UnexpectedLoadErr # fp