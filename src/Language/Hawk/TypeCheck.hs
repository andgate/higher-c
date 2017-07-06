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
  #-}
module Language.Hawk.TypeCheck where

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.Default.Class
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Set (Set, (\\))
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Syntax
import Language.Hawk.TypeCheck.NameGen

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T



newtype TypeEnv = TypeEnv (Map Var Scheme)
data Scheme = Scheme [TVar] Type
type Subst = Map TVar Type

data InferState
  = InferState
      { _inferSupply :: Int
      , _inferSubst :: Subst
      }

makeClassy ''InferState

instance Default InferState where
    def = InferState 0 nullSubst

newtype InferT m a = InferT { unInferT :: StateT InferState m a }
  deriving (Functor, Applicative, Monad, MonadState InferState)

evalInferT :: Monad m => InferT m a -> m a
evalInferT = flip evalStateT def . unInferT

newTVar :: (MonadState s m, HasInferState s)
        => Text -> m Type 
newTVar prefix = do
  i <- pack . show <$> use inferSupply
  inferSupply += 1
  return . TVar . TypeVar $ prefix `T.append` i  

class HasFreeTVars a where
    freeTVars :: a -> Set TVar

instance HasFreeTVars Type where
    freeTVars = \case
      TCon _     -> Set.empty
      TVar v    -> Set.singleton v
      TFun a b  -> freeTVars a `Set.union` freeTVars b

instance HasFreeTVars Scheme where
    freeTVars (Scheme tvs t) = freeTVars t \\ Set.fromList tvs

instance (HasFreeTVars a) => HasFreeTVars [a] where
    freeTVars = foldr Set.union Set.empty . map freeTVars

instance HasFreeTVars TypeEnv where
    freeTVars (TypeEnv env) = freeTVars $ Map.elems env


class Substitutable a where
    apply :: Subst -> a -> a

instance Substitutable Type where
    apply s = \case
      TVar n    -> case Map.lookup n s of
                          Nothing -> TVar n
                          Just t  -> t

      TFun a b  -> TFun (apply s a) (apply s b)
      t         -> t


instance Substitutable Scheme where
    apply s (Scheme vs t) = Scheme vs (apply (foldr Map.delete s vs) t)


instance (Substitutable a) => Substitutable [a] where
    apply s = map (apply s)

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)


-- Substitution Helpers
nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1


-- Type environment helpers
remove :: TypeEnv -> Var -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)


-- The core of the Algorithm W
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vs t
  where vs = Set.toList $ freeTVars t \\ freeTVars env


instantiate :: (MonadState s m, HasInferState s)
            => Scheme -> m Type
instantiate (Scheme vars t) = do
  nvars <- mapM (\_ -> newTVar "a") vars
  let s = Map.fromList (zip vars nvars)
  return $ apply s t


unify :: (MonadState s m, HasInferState s)
      => (Type, Type) -> m Subst
unify = \case
  (TFun a1 b1, TFun a2 b2) -> do
      s1 <- unify (a1, a2)
      s2 <- unify (apply s1 b1, apply s1 b2)
      return (s1 `composeSubst` s2)
  
  (TVar n, t) -> varBind n t
  (t, TVar n) -> varBind n t

  (TCon n1, TCon n2)
    | n1 == n2 -> return nullSubst 
    | otherwise -> error "Types do not unify"

  (t1, t2) -> error "Types do not unify"


varBind :: (MonadState s m, HasInferState s)
        => TVar -> Type -> m Subst
varBind n t
  | t == TVar n                 = return nullSubst
  | n `Set.member` freeTVars t  = error "Occurs check failed"
  | otherwise                   = return $ Map.singleton n t


inferLit :: (MonadState s m, HasInferState s)
         => Lit -> m (Subst, Type)
inferLit (IntLit v)   = return (nullSubst, TCon . Name $ "Integer")
inferLit (FloatLit v) = return (nullSubst, TCon . Name $ "Float")
inferLit (CharLit v)  = return (nullSubst, TCon . Name $ "Char")
inferLit (BoolLit v)  = return (nullSubst, TCon . Name $ "Bool")


inferExp :: (MonadState s m, HasInferState s)
         => TypeEnv -> ExpRn -> m (ExpTc, Subst, Type)
inferExp env@(TypeEnv envMap) = \case
  ELit loc lit -> do
    (s, t) <- inferLit lit
    return (ELit (t, loc) lit, s, t)

  EVar loc n ->
    case Map.lookup n envMap of
      Nothing -> error "unbound variable"
      Just sigma -> do  t <- instantiate sigma
                        return (EVar (t, loc) n, nullSubst, t)

  ECon loc n ->
    case Map.lookup n envMap of
      Nothing -> error "unbound variable"
      Just sigma -> do  t <- instantiate sigma
                        return (ECon (t, loc) n, nullSubst, t)

  EPrim loc instr ->
    error "primitive instructions are not type checked yet"

  EApp loc a b -> do
    tv <- newTVar "a"
    (a', s1, t1) <- inferExp env a
    (b', s2, t2) <- inferExp (apply s1 env) b
    s3 <- unify (apply s2 t1, TFun t2 tv)
    let s' = s3 `composeSubst` s2 `composeSubst` s1
        t' = apply s3 tv
        e' = EApp (t',loc) a' b' 
    return (e', s', t')

  ELam loc n e -> do
    tv <- newTVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv $ env' `Map.union` Map.singleton n (Scheme [] tv)
    (e', s, t) <- inferExp env'' e
    let t' = TFun (apply s tv) t
    return (ELam (t', loc) n e', s, t')

  EIf loc pred e1 e2 ->
    error "if expression not supported"

  ELet loc n e1 e2 -> do
    (e1', s1, t1) <- inferExp env e1
    let TypeEnv env' = remove env n
        t' = generalize (apply s1 env) t1
        env'' = TypeEnv (Map.insert n t' env')
    (e2', s2, t2) <- inferExp (apply s1 env'') e2
    let e' = ELet (t2, loc) n e1' e2'
        s' = s1 `composeSubst` s2
    return (e', s', t2)

  EDup loc e -> do
    (e', s, t) <- inferExp env e
    return (EDup (t, loc) e', s, t)

  EDrop loc v e -> do
    (e', s, t) <- inferExp env e
    return (EDrop (t, loc) v e', s, t)

  Exp _ -> error "Expression extension is not supported by typechecker."