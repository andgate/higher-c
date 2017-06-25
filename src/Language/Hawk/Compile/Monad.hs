{-# LANGUAGE  FlexibleInstances
            , FlexibleContexts
            , GeneralizedNewtypeDeriving
            , MultiParamTypeClasses
            , OverloadedStrings
            , TypeFamilies
            , UndecidableInstances
  #-}
module Language.Hawk.Compile.Monad where


import Control.Lens
import Control.Monad.Except
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Control.Monad.Catch
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Semigroup
import Data.These
import Data.Default.Class
import Language.Hawk.Compile.Config
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Language.Hawk.Compile.State
import System.IO



-------------------------------------------------------------------------------
-- Compiler Monad
newtype Hkc a = Hkc { unHkc :: StateT HkcState (ReaderT HkcConfig (ChronicleT [WithTimestamp HkcErr] (LoggingT (WithSeverity (WithTimestamp HkcMsg)) IO))) a }
    deriving
     ( Functor, Applicative, Monad 
     , MonadState   HkcState
     , MonadReader  HkcConfig
     , MonadLog (WithSeverity (WithTimestamp HkcMsg))
     , MonadChronicle [WithTimestamp HkcErr]
     , MonadBase IO
     , MonadIO
     , MonadThrow
     , MonadCatch
     )


runHkc :: Hkc a -> HkcConfig -> IO ()
runHkc m conf =
  void $
    runLoggingT (logErrors =<< runChronicleT (runReaderT (runStateT (unHkc m) def) conf))
                               (\msg -> liftIO $ print msg)     
      
          

logErrors :: (MonadLog (WithSeverity (WithTimestamp HkcMsg)) m)
          => These [WithTimestamp HkcErr] a -> m ()
logErrors = 
  these (\errs -> mapM_ logErr errs)
        (\_ -> return ())
        (\errs _ -> mapM_ logErr errs)
  where
    logErr err = 
      logError $ fmap HkcErrMsg err



-------------------------------------------------------------------------------
-- Helper instances

instance MonadBaseControl IO Hkc where
    type StM Hkc a = These [WithTimestamp HkcErr] (a, HkcState)
    liftBaseWith f = Hkc $ liftBaseWith $ \q -> f (q . unHkc)
    restoreM = Hkc . restoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}


{--
runHkc
  :: HkcState
  -> Compiler ()
  -> IO ()
runCompiler = flip evalStateT

--}

