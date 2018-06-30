{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , GeneralizedNewtypeDeriving
           , TemplateHaskell
  #-}
module Language.Hawk.Rename where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Map
import Data.Set
import Data.Text (Text)
import Language.Hawk.Rename.Error

import qualified Language.Hawk.Syntax.Source as S
import qualified Language.Hawk.Syntax.Abstract as A

data REnv
  = REnv
    { _envGlobal :: Set Text
    , _envLocals :: Map Text [Int]
    }

makeLenses ''REnv

newtype Rn a = Rn { unRn :: StateT Int (ReaderT REnv (Except RnError)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader REnv
           , MonadState Int
           , MonadError RnError
           )

runRn :: Int -> REnv -> Rn a -> Either RnError (a, Int)  
runRn n env r = runExcept (runReaderT (runStateT (unRn r) n) env)

renameExp :: Int -> REnv -> S.Exp -> Either RnError (A.TermInf, Int)
renameExp n env = runRn n env . rnExp

rnExp :: S.Exp -> Rn A.TermInf
rnExp = \case
  S.EVar n -> undefined
  _ -> undefined