{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Compile.Monad where


import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Except
import Data.Binary
import Data.Data
import Data.Monoid
import Data.Text.Lazy (Text)
import Data.Typeable
import Database.Persist.TH

import qualified Data.Text.Lazy as L
import qualified Language.Hawk.Syntax.Module as M



-------------------------------------------------------------------------------
-- Compiler Monad

-- | Main compiler driver a monad.
type Compiler = StateT CompilerState IO


-- | Run the compiler pipeline.
runCompiler
  :: CompilerState
  -> Compiler ()
  -> IO ()
runCompiler = flip evalStateT


-------------------------------------------------------------------------------
-- Package Information

type Packages = [Package]

data Package =
  Package Text [FilePath]
  deriving (Eq, Show, Read, Ord, Data, Typeable)

-------------------------------------------------------------------------------
-- Compiler State

data CompilerState = 
  CompilerState
  { packages :: Packages
  , currentStage :: CompilerPhase
  } deriving (Eq, Show, Read, Ord, Data, Typeable)
  
data CompilerPhase =
    InitialPhase
  | MetadataCollectionPhase
  | TableGenerationPhase
  | ExpressionCollectionPhase
  | NameCheckingPhase
  | TypeInferencePhase
  | TypeCheckingPhase
  | CodeGenerationPhase
  | BinaryGenerationPhase
  deriving (Eq, Show, Read, Ord, Data, Typeable)
  
derivePersistField "CompilerPhase"