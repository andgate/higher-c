{-# LANGUAGE  GeneralizedNewtypeDeriving
            , TemplateHaskell
            , OverloadedStrings
  #-}
module Language.Hawk.TypeCheck.NameGen where

import Control.Lens
import Control.Monad
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.Trans
import Data.Text (Text, pack, unpack)
import Debug.Trace
import Language.Hawk.Syntax
import Text.PrettyPrint.Leijen.Text (Pretty, pretty)

import qualified Text.PrettyPrint.Leijen.Text as PP


data NameState
  = NameState
    { _nameVars   :: [String]
    , _nameTVars  :: [String]
    , _nameIndent :: Int -- This has no place here, but useful for debugging
    }

makeClassy ''NameState

initialNameState :: NameState
initialNameState
  = NameState
    { _nameVars  = map ('$':) namelist
    , _nameTVars = map ('\'':) namelist
    , _nameIndent    = 0
    }
  where namelist = [1..] >>= flip replicateM ['a' .. 'z']


newtype NameGenT m a = NameGenT { unNameGenT :: StateT NameState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadState NameState)

evalNameGenT :: Monad m => NameGenT m a -> m a
evalNameGenT = (flip evalStateT) initialNameState . unNameGenT


-- Create a fresh variable
freshVar :: (MonadState s m, HasNameState s)
         => m Var
freshVar = do
  v:vs <- use nameVars
  nameVars .= vs
  return . Var . pack $  v

-- Create a fresh type variable
freshTVar :: (MonadState s m, HasNameState s)
          => m TVar
freshTVar = do
  v:vs <- use nameTVars
  nameTVars .= vs
  return . TypeVar . pack $ v