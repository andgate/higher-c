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
            , TypeSynonymInstances
            , UndecidableInstances
            , StandaloneDeriving
  #-}
module Language.Hawk.TypeCheck where

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Except
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
import Language.Hawk.TypeCheck.Assumption (Assumption)
import Language.Hawk.TypeCheck.Environment (Env)
import Language.Hawk.TypeCheck.Error
import Language.Hawk.TypeCheck.State
import Language.Hawk.TypeCheck.Types

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T
import qualified Language.Hawk.TypeCheck.Assumption as As
import qualified Language.Hawk.TypeCheck.Environment as Env



-----------------------------------------------------------------------
-- Classes
-----------------------------------------------------------------------

newtype Infer a = Infer { unInfer :: ReaderT (Set Text) (StateT InferState (Except TcErr)) a }
  deriving (Functor, Applicative, Monad, MonadReader (Set Text), MonadState InferState, MonadError TcErr)


class Substitutable a where
  apply :: Subst -> a -> a


instance Substitutable Type where
  apply s@(Subst s_map) = \case
    t@(TVar a)   -> Map.findWithDefault t a s_map
    TCon n       -> TCon n
    TApp t1 t2   -> apply s t1 `TApp` apply s t2
    TArr t1 t2   -> apply s t1 `TArr` apply s t2
    TLoli t1 t2  -> apply s t1 `TLoli` apply s t2
    TKind k t    -> TKind k $ apply s t
    TLoc l t     -> TLoc l $ apply s t
    TParen t     -> TParen $ apply s t


instance Substitutable Scheme where
  apply s@(Subst s_map) (Forall as t) = Forall as $ apply s t
    where s' = Subst $ foldr Map.delete s_map as


instance Substitutable Constraint where
  apply s = \case
    EqConst t1 t2          -> EqConst (apply s t1) (apply s t2)
    ExpInstConst t sc      -> ExpInstConst (apply s t) (apply s sc)
    ImpInstConst t1 ms t2  -> ImpInstConst (apply s t1) (apply s ms) (apply s t2)


instance Substitutable a => Substitutable [a] where
  apply = map . apply


instance (Ord a, Substitutable a) => Substitutable (Set a) where
  apply = Set.map . apply



class FreeTypeVars a where
  ftv :: a -> Set Text


instance FreeTypeVars Type where
  ftv = \case
    TVar a -> Set.singleton a
    TCon _ -> Set.empty
    TApp t1 t2   -> ftv t1 `Set.union` ftv t2
    TArr t1 t2   -> ftv t1 `Set.union` ftv t2
    TLoli t1 t2  -> ftv t1 `Set.union` ftv t2
    TKind _ t    -> ftv t
    TLoc _ t     -> ftv t
    TParen t     -> ftv t


instance FreeTypeVars Scheme where
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as


instance FreeTypeVars a => FreeTypeVars [a] where
  ftv = foldr (Set.union . ftv) Set.empty

instance FreeTypeVars a => FreeTypeVars (Set a) where
  ftv = foldr (Set.union . ftv) Set.empty


class ActiveTypeVars a where
  atv :: a -> Set Text

instance ActiveTypeVars Constraint where
  atv = \case
    EqConst t1 t2          -> ftv t1 `Set.union` ftv t2
    ImpInstConst t1 ms t2  -> ftv t1 `Set.union` (ftv ms `Set.intersection` ftv t2) 
    ExpInstConst t s       -> ftv t `Set.union` ftv s 


instance ActiveTypeVars a => ActiveTypeVars [a] where
  atv = foldr (Set.union . atv) Set.empty


-----------------------------------------------------------------------
-- Inference
-----------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Infer a -> Either TcErr a
runInfer (Infer m) = runExcept $ evalStateT (runReaderT m Set.empty) initInfer


inferType :: Env -> Exp -> Infer (Subst, Type)
inferType env ex = do
  (as, cs, t) <- infer ex
  let unbounds = Set.fromList (As.keys as) `Set.difference` Set.fromList (Env.keys env)
  unless (Set.null unbounds) $ throwError $ UnboundVariable (Set.findMin unbounds)
  let cs' = [ExpInstConst t s | (x, s) <- Env.toList env, t <- As.lookup x as]
  return undefined
  -- subst <- solve (cs ++ cs')
  -- return (subst, apply subst t)


-- | Solve for the toplevel type of an expression
inferExp :: Env -> Exp -> Either TcErr Scheme
inferExp env ex = case runInfer (inferType env ex) of
  Left err -> Left err
  Right (subst, ty) -> Right $ closeOver $ apply subst ty


-- | Cannonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize Set.empty


extendMSet :: Text -> Infer a -> Infer a
extendMSet x = local (Set.insert x)


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

genFreeVar :: Int -> Type
genFreeVar n =
  TVar $ pack (letters !! n)

fresh :: Infer Type
fresh = do
  countfv += 1
  uses countfv genFreeVar


instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s t


generalize :: Set Text -> Type -> Scheme
generalize free t = Forall as t
  where as = Set.toList $ ftv t `Set.difference` free


ops :: Operator -> Type


infer :: Exp -> Infer (Assumption, [Constraint], Type)
infer = \case
  _ -> undefined

