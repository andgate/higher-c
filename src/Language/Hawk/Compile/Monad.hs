{-# LANGUAGE  FlexibleInstances
            , GeneralizedNewtypeDeriving
            , MultiParamTypeClasses
            , OverloadedStrings
            , StandaloneDeriving
            , TemplateHaskell
            , TypeFamilies
            , UndecidableInstances
  #-}
module Language.Hawk.Compile.Monad where


import Control.Monad.Except
import Control.Monad.Chronicle
import Control.Monad.Catch
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Base
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Data.Semigroup
import Data.These
import Language.Hawk.Compile.Config
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Language.Hawk.Compile.State



-------------------------------------------------------------------------------
-- Compiler Monad
newtype Hkc a = Hkc { unHkc :: StateT HkcState (ReaderT HkcConfig (LoggingT (WithSeverity HkcMessage) (ChronicleT [HkcErr] IO))) a }
    deriving
     ( Functor, Applicative, Monad 
     , MonadState   HkcState
     , MonadReader  HkcConfig
     , MonadLog (WithSeverity HkcMessage)
     , MonadChronicle [HkcErr]
     , MonadBase IO
     , MonadIO
     , MonadThrow
     , MonadCatch
     )




-------------------------------------------------------------------------------
-- Helper instances
instance MonadChronicle c m => MonadChronicle c (LoggingT msg m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (LoggingT m) = LoggingT $ memento m
    absolve x (LoggingT m) = LoggingT $ absolve x m
    condemn (LoggingT m) = LoggingT $ condemn m
    retcon f (LoggingT m) = LoggingT $ retcon f m
    chronicle = lift . chronicle

instance (Semigroup c, MonadThrow m) => MonadThrow (ChronicleT c m) where
    throwM e = lift $ throwM e

instance (Semigroup c, MonadCatch m) => MonadCatch (ChronicleT c m) where
    catch (ChronicleT m) h = ChronicleT $ m `catch` (\e -> runChronicleT (h e))

instance MonadChronicle c m => MonadChronicle c (ResourceT m)
instance MonadLog msg m => MonadLog msg (ResourceT m)

instance (Semigroup c) => MonadTransControl (ChronicleT c) where
    type StT (ChronicleT c) a = These c a
    liftWith f = ChronicleT $ liftM return $ f $ runChronicleT
    restoreT = ChronicleT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (Semigroup c, MonadBaseControl b m) => MonadBaseControl b (ChronicleT c m) where
    type StM (ChronicleT c m) a = ComposeSt (ChronicleT c) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

instance (Semigroup c, MonadBase b m) => MonadBase b (ChronicleT c m) where
    liftBase = lift . liftBase
    {-# INLINABLE liftBase #-}

instance MonadBaseControl IO Hkc where
    type StM Hkc a = These [HkcErr] (a, HkcState)
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

