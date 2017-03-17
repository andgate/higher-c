module Language.Hawk.Validate.Monad where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Except
import Control.Monad.Except
import Data.Binary
import Data.Data
import Data.Monoid
import Data.Text.Lazy (Text)
import Data.Typeable
import Database.Persist.TH

import qualified Data.Text.Lazy as L
import qualified Language.Hawk.Compile.Monad as C
import qualified Language.Hawk.Syntax.Module as M


-------------------------------------------------------------------------------
-- Validator Monad

type Validator = StateT ValidatorState IO


-- | Run the validator pipeline.
runValidator
    :: ValidatorState
    -> Validator ()
    -> IO ()
runValidator = flip evalStateT


-------------------------------------------------------------------------------
-- Validator State

data ValidatorState = 
  VState
  { compilerState :: C.CompilerState
  , currentStage :: ValidatorPhase
  } deriving (Eq, Show, Read, Ord, Data, Typeable)
  
  
data ValidatorPhase =
    InitialPhase
  | ModuleSubscriptionPhase
  | NameDistributionPhase
  | TopLevelNameValidationPhase
  | OperatorTableConstructionPhase
  | ExpressionConstructionPhase
  | TypeConstructionPhase
  deriving (Eq, Show, Read, Ord, Data, Typeable)
  
derivePersistField "CompilerPhase"