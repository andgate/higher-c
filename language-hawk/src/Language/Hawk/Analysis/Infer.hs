{-# Language GeneralizedNewtypeDeriving, LambdaCase #-}
module Language.Hawk.Analysis.Infer where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map.Strict (Map)
import Data.Text (Text)
import Language.Hawk.Analysis.Equality
import Language.Hawk.Syntax.Abstract
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Location
import Unbound.Generics.LocallyNameless


import qualified Data.Map.Strict as M



data TcError
  = TypeMismatch Type Type


data Hint = Hint TName Term

data TcEnv
  = TcEnv
    { _tcDict :: Map Text Type
    , _tcHints :: [Hint]
    , _tcLoc :: Location
    }


newtype Tc a = Tc { unTc :: ReaderT TcEnv (ExceptT TcError FreshM) a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader TcEnv
           , MonadError TcError
           , Fresh
           )


runTc :: TcEnv -> Tc a -> Either TcError a
runTc env tc = runFreshM $ runExceptT $ runReaderT (unTc tc) env


typecheck :: Term -> TcEnv -> Either TcError (Term, Type)
typecheck t env = runTc env (infer t)


check :: Term -> Maybe Type -> Tc (Type, Term)
check tm ty = do
  nf <- whnf ty
  check' tm (Just nf)


infer :: Term -> Tc (Term, Type)
infer m = check t Nothing



check' :: (Term, Maybe Type) -> Tc (Type, Term)
check' = \case
  (TVar x, Nothing) -> do
    ty <- lookupTy x
    return ty