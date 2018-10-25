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
import Control.Monad.Chronicle.Extra ()
import Control.Monad.Catch
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Bag
import Data.Time.Format
import Data.These
import Data.Default.Class
import Language.Hawk.Compile.Config
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Text.PrettyPrint.Leijen.Text
import System.IO


-------------------------------------------------------------------------------
-- Compiler Monad
newtype Hkc a = Hkc { unHkc :: ChronicleT (Bag HkcErr) (LoggingT (WithSeverity HkcMsg) IO) a }
    deriving
     ( Functor, Applicative, Monad
     , MonadLog (WithSeverity HkcMsg)
     , MonadChronicle (Bag HkcErr)
     , MonadBase IO
     , MonadIO
     , MonadThrow
     , MonadCatch
     )


runHkc :: Hkc a -> IO ()
runHkc m =
  void $
    withFDHandler defaultBatchingOptions stderr 0.4 80 $ \stderrHandler ->
    withFDHandler defaultBatchingOptions stdout 0.4 80 $ \stdoutHandler ->
    runLoggingT (logErrors =<< runChronicleT (unHkc m))
                (\msg ->
                    case msgSeverity msg of
                      Error -> stderrHandler $ pretty $ discardSeverity msg                  
                      _     -> stdoutHandler $ renderWithSeverity pretty msg
                )
      
          

logErrors :: (MonadLog (WithSeverity HkcMsg) m)
          => These (Bag HkcErr) a -> m ()
logErrors = 
  these (\errs -> mapM_ logErr errs)
        (\_ -> return ())
        (\errs _ -> mapM_ logErr errs)
  where
    logErr err = 
      logError $ HkcErrMsg err



-------------------------------------------------------------------------------
-- Helper instances

instance MonadBaseControl IO Hkc where
    type StM Hkc a = These (Bag HkcErr) a
    liftBaseWith f = Hkc $ liftBaseWith $ \q -> f (q . unHkc)
    restoreM = Hkc . restoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}
