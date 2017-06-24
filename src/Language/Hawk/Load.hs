{-# LANGUAGE  FlexibleContexts #-}
module Language.Hawk.Load where

import Control.Lens
import Control.Monad.Chronicle
import Control.Monad.Log
import Control.Monad.State
import Control.Monad.Reader
import Language.Hawk.Compile.Config
import Language.Hawk.Compile.State
import Language.Hawk.Load.Error
import Language.Hawk.Load.Message

load
  :: ( MonadState s m, HasHkcState s
     , MonadReader c m, HasHkcConfig c
     , MonadLog (WithSeverity msg) m, AsLoadMsg msg
     , MonadChronicle [e] m, AsLoadErr e
     , MonadIO m
     )
  => m ()
load =
    undefined