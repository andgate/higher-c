module Language.Hawk.Compile.Monad where


import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Except
import Data.Monoid
import Data.Text.Lazy (Text)

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
-- Compiler State

data CompilerState = 
  CompilerState
  { packageName :: Text
  , srcFiles :: [FilePath]
  , currentStage :: CompilerPhase
  }
  
  
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