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
import Data.List (lookup, union, concatMap, nub, find, delete, intersect)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding ((<>))
import Data.Set (Set, (\\))
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Compile.State
import Language.Hawk.Syntax
import Language.Hawk.TypeCheck.Error
import Language.Hawk.TypeCheck.State

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T


type Subst = [(Text, Type)]

nullSubst :: Subst
nullSubst = []

(+->) :: Text -> Type -> Subst
(+->) v ty = [(v, ty)]


class Types t where
  apply :: Subst -> t -> t
  tv :: t -> [Text]


instance Types Type where
  apply s = \case
    TVar v -> case lookup v s of
                Just t  -> t
                Nothing -> TVar v

    TApp a b -> TApp (apply s a) (apply s b)
    TArr a b -> TArr (apply s a) (apply s b)
    TLoli a b -> TLoli (apply s a) (apply s b)

    TKind k ty -> TKind k (apply s ty)
    TLoc  l ty -> TLoc l (apply s ty)
    TParen ty  -> TParen (apply s ty)

    t -> t

  tv = \case
    TVar v -> [v]

    TApp a b -> tv a `union` tv b
    TArr a b -> tv a `union` tv b
    TLoli a b -> tv a `union` tv b

    TKind _ ty -> tv ty
    TLoc _ ty -> tv ty
    TParen ty -> tv ty

    _ -> []



instance Types a => Types [a] where
  apply s = map (apply s)

  tv = nub . concatMap tv


instance Types t => Types (Qual t) where
  apply s (ps :=> t)  = apply s ps :=> apply s t
  tv (ps :=> t)       = tv ps `union` tv t


instance Types Pred where
  apply s (IsIn i t)  = IsIn i (apply s t)
  tv (IsIn i t)       = tv t


infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1


merge :: ( MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
         )
      => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1++s2) else undefined
  where
    agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                (map fst s1 `intersect` map fst s2)


mgu :: ( MonadState s m, HasTCState s
       , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
       )
    => (Type, Type) -> m Subst
mgu = \case
  (TApp l r, TApp l' r') -> do
    s1 <- mgu (l, l')
    s2 <- mgu (apply s1 r, apply s1 r')
    return (s2 @@ s1)
  (TVar n, t) -> varBind n t
  (t, TVar n) -> varBind n t
  (TCon n1, TCon n2)
    | n1 == n2 -> return nullSubst

  (t1, t2) -> undefined -- types do not unify
                              
                              
varBind :: ( MonadState s m, HasTCState s
           , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
           )
        => Text -> Type -> m Subst
varBind n ty
  | ty == TVar n       = return nullSubst
  | n `elem` tv ty     = undefined -- occurs check fails
--  | kind n /= kind ty  = undefined -- kinds do not match
  | otherwise          = return (n +-> ty)


match :: ( MonadState s m, HasTCState s
         , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
         )
      => (Type, Type) -> m Subst
match = \case
  (TApp l r, TApp l' r') -> do
    sl <- match (l, l')
    sr <- match (r, r')
    merge sl sr

--  (TVar n, t)
--    | kind n == kind t -> return nullSubst

  (TCon n1, TCon n2)
    | n1 == n2 -> return nullSubst

  (t1, t2) -> undefined -- types do not match



mguPred :: ( MonadState s m, HasTCState s
           , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
           )
        => Pred -> Pred -> m Subst
mguPred = liftPred mgu


matchPred :: ( MonadState s m, HasTCState s
             , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
             )
          => Pred -> Pred -> m Subst
matchPred = liftPred match


liftPred :: ( MonadState s m, HasTCState s
            , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
            )
         => ((Type,Type) -> m Subst) -> Pred -> Pred -> m Subst
liftPred m (IsIn i ts1) (IsIn i' ts2)
  | i == i' = foldM merge nullSubst =<< zipWithM (curry m) ts1 ts2
  | otherwise = undefined -- classes differ
