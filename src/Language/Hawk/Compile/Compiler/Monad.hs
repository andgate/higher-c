{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Hawk.Compile.Compiler.Monad where


import Data.Monoid
import qualified Data.Text.Lazy as L

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except

import qualified Language.Hawk.Syntax.Module as M


-------------------------------------------------------------------------------
-- Compiler Monad

type CompilerMonad =
  ExceptT Msg
    (StateT CompilerState IO)

-- | Main compiler driver a monad.
newtype CompilerM a = Compiler { runCompiler :: CompilerMonad a }
  deriving
  ( Functor
  , Applicative
  , Alternative
  , Monad
  , MonadFix
  , MonadPlus
  , MonadIO
  , MonadState CompilerState
  , MonadError Msg
  )


-- | Run the compiler pipeline.
runCompilerM
  :: CompilerM a
  -> CompilerState
  -> IO (Either Msg a, CompilerState)
runCompilerM = runStateT . runExceptT . runCompiler

-- This needs to be moved into report
type Msg = String

-------------------------------------------------------------------------------
-- Compiler State

data CompilerState = CompilerState
  { _fnames   :: [FilePath]
  , _imports  :: [FilePath]
  , _ast      :: M.Source
  } deriving (Eq, Show)