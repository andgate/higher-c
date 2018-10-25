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
import Data.Default.Instances.Text
import Data.List (nub, groupBy)
import Data.Map (Map)
import Data.Text (Text)
import Language.Hawk.Load.Error
import Language.Hawk.Load.Message
import Language.Hawk.Load.Result (LdResult)
import System.IO.Error

import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as T
import qualified Language.Hawk.Load.Result as R

loadFiles
  :: ( MonadLog (WithSeverity msg) m, AsLdMsg msg
     , MonadChronicle (Bag e) m, AsLdErr e
     , MonadIO m
     )
  => [FilePath] -> m LdResult
loadFiles fps =
  foldM (\r fp -> mappend r <$> loadFile fp)
        mempty
        (nub fps)


loadFile
    ::  ( MonadLog (WithSeverity msg) m, AsLdMsg msg
        , MonadChronicle (Bag e) m, AsLdErr e
        , MonadIO m
        )
  => FilePath -> m LdResult
loadFile fp = do
  srcOrExc <- liftIO . try . T.readFile $ fp
  case srcOrExc of
      Left ex ->
        disclose $ One $ mkLoadErr fp ex
        
      Right src -> do
        logInfo (_FileFound # fp)
        return $ R.singleton fp src


mkLoadErr :: (AsLdErr e) => FilePath -> IOException -> e
mkLoadErr fp ex
  | isDoesNotExistError ex =
      _FileNotFound # fp

  | isAlreadyInUseError ex =
      _FileInUse # fp

  | isPermissionError ex   =
      _PermissionDenied # fp

  | otherwise              =
      _UnexpectedLoadErr # fp
