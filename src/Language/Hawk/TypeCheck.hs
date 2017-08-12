{-# LANGUAGE  OverloadedStrings
            , FlexibleInstances
            , FlexibleContexts
            , GADTs
            , LambdaCase
            , MultiParamTypeClasses
            , FunctionalDependencies
            , RankNTypes
            , TemplateHaskell
            , GeneralizedNewtypeDeriving
            , UndecidableInstances
            , StandaloneDeriving
  #-}
module Language.Hawk.TypeCheck where

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Control.Monad.Log
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, local)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.Bag
import Data.Default.Class
import Data.List (lookup, nub, find, delete)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding ((<>))
import Data.Set (Set, (\\))
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Compile.State
import Language.Hawk.Syntax hiding (Type (..), Var, Kind)
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Term
import Language.Hawk.TypeCheck.Error
import Language.Hawk.TypeCheck.Environment
import Language.Hawk.TypeCheck.Equality

import Unbound.Generics.LocallyNameless

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T

inferType :: (MonadReader r m, HasTcEnv r, Fresh m) 
          => Term -> m (Term, Type)
inferType t = tcTerm (t, Nothing)

checkType :: (MonadReader r m, HasTcEnv r, Fresh m)
          => Term -> Type -> m (Term, Type)
checkType tm expected = do
  nf <- whnf expected
  tcTerm (tm, Just nf)


tcTerm :: (MonadReader r m, HasTcEnv r, Fresh m)
       => (Term, Maybe Type) -> m (Term, Type)
tcTerm = \case
  (t@(Var x), Nothing) -> undefined
  (Pi bnd, Nothing) -> undefined
  (Lam bnd, Just (Pi bnd2)) -> undefined
  (Lam _, Just nf) -> undefined
  (Lam bnd, Nothing) -> undefined
  (App t1 t2, Nothing) -> undefined

  (Lit lit, Nothing) -> undefined
  (TyLit tlit, Nothing) -> undefined
  (Prim i, Nothing) -> undefined

  (Ann tm ty, Nothing) -> undefined
  (Paren tm, mTy)     -> undefined
  (TLoc l tm, mTy)     -> undefined
  (Hole, mTy) -> undefined

  (t@(Kind _), Nothing) -> undefined
  _ -> undefined