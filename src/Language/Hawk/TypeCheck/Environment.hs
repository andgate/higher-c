{-# Language  TemplateHaskell #-}
module Language.Hawk.TypeCheck.Environment where

import Control.Lens
import Control.Monad.Reader
import Data.Maybe (listToMaybe)
import Language.Hawk.Syntax.Decl
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Term
import Unbound.Generics.LocallyNameless



newtype Tc m a = Tc { unTc :: FreshMT (ReaderT TcEnv m) a }

runTc :: Monad m => TcEnv -> Tc m a -> m a
runTc env m = runReaderT (runFreshMT (unTc m)) env


data Hint = Hint TName Term


data TcEnv
  = TcEnv { _tcCtx      :: [Decl]
          , _tcGlobals  :: Int
          , _tcHints    :: [Hint]
          , _tcSrcLoc   :: [Location] 
          }

makeClassy ''TcEnv


lookupDef :: (MonadReader r m, HasTcEnv r)
          => TName -> m (Maybe Term)
lookupDef v =
  views tcCtx $ \ctx ->
    listToMaybe [a | Def v' a <- ctx, v == v']

lookupRecDef :: (MonadReader r m, HasTcEnv r)
          => TName -> m (Maybe Term)
lookupRecDef v =
  views tcCtx $ \ctx ->
    listToMaybe [a | RecDef v' a <- ctx, v == v']
