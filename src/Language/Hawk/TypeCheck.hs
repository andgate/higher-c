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
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.Bag
import Data.Default.Class
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.Set (Set, (\\))
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Compile.State
import Language.Hawk.Syntax
import Language.Hawk.Syntax.Prim
import Language.Hawk.TypeCheck.Error
import Language.Hawk.TypeCheck.NameGen

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T



newtype TypeEnv = TypeEnv (Map Var Scheme)
data Subst = Subst (Map TVar Type)


-- Inference State
data InferState
  = InferState
      { _inferSupply :: Int
      , _inferSubst :: Subst
      }

makeClassy ''InferState

instance Default InferState where
    def = InferState 0 nullSubst

-- Inference Monad Transformer
newtype InferT m a = InferT { unInferT :: StateT InferState m a }
  deriving (Functor, Applicative, Monad, MonadState InferState, MonadTrans)

evalInferT :: Monad m => InferT m a -> m a
evalInferT = flip evalStateT def . unInferT

newTVar :: (MonadState s m, HasInferState s)
        => Text -> m Type 
newTVar prefix = do
  i <- pack . show <$> use inferSupply
  inferSupply += 1
  return . TVar . TypeVar $ prefix `T.append` i  

deriving instance MonadIO m => MonadIO (InferT m)

instance MonadChronicle c m => MonadChronicle c (InferT m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (InferT m) = InferT $ memento m
    absolve x (InferT m) = InferT $ absolve x m
    condemn (InferT m) = InferT $ condemn m
    retcon f (InferT m) = InferT $ retcon f m
    chronicle = lift . chronicle


-- Free Type Variables
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


-- Substitubles
class Substitutable a where
    apply :: Subst -> a -> a

instance Substitutable Type where
    apply s = \case
      TVar n    -> case findSubst n s of
                          Nothing -> TVar n
                          Just t  -> t

      TFun a b  -> TFun (apply s a) (apply s b)
      t         -> t


instance Substitutable Scheme where
    apply s (Scheme vs t) = Scheme vs (apply (foldr removeSubst s vs) t)


instance (Substitutable a) => Substitutable [a] where
    apply s = map (apply s)

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)


-- Substitution Helpers
nullSubst :: Subst
nullSubst = Subst $ Map.empty

subst :: TVar -> Type -> Subst
subst var t = Subst $ Map.singleton var t

substs :: [(TVar, Type)] -> Subst
substs = Subst . Map.fromList

composeSubst :: Subst -> Subst -> Subst
composeSubst subst1@(Subst s1) (Subst s2)
  = Subst $ (Map.map (apply subst1) s2) `Map.union` s1

findSubst :: TVar -> Subst -> Maybe Type
findSubst var (Subst s) = Map.lookup var s

removeSubst :: TVar -> Subst -> Subst
removeSubst tvar (Subst s) = Subst $ Map.delete tvar s



instance Default Subst where
    def = nullSubst

-- Need to newtype Subst to get this
instance Monoid Subst where
    mempty = nullSubst
    mappend = composeSubst

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
  let s = Subst $ Map.fromList (zip vars nvars)
  return $ apply s t


unify :: ( MonadState s m, HasInferState s
         , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
         , MonadIO m
         )
      => (Maybe Location, Maybe Location) -> (Type, Type) -> m Subst
unify (loc1, loc2) = \case
  (TFun a1 b1, TFun a2 b2) -> do
      s1 <- unify (loc1, loc2) (a1, a2)
      s2 <- unify (loc1, loc1) (apply s1 b1, apply s1 b2)
      return (s1 `composeSubst` s2)
  
  (TVar n, t) -> varBind n loc1 t loc2
  (t, TVar n) -> varBind n loc1 t loc2

  (TCon n1, TCon n2)
    | n1 == n2 -> return nullSubst 
    | otherwise -> discloseNow (_UnificationFailure # (TCon n1, loc1, TCon n2, loc2))

  (t1, t2) -> discloseNow (_UnificationFailure # (t1, loc1, t2, loc2))


varBind :: ( MonadState s m, HasInferState s
           , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
           , MonadIO m
           )
        => TVar -> Maybe Location -> Type -> Maybe Location -> m Subst
varBind n ln t lt
  | t == TVar n                 = return nullSubst
  | n `Set.member` freeTVars t  = discloseNow (_OccursCheckFail # (n, ln, t, lt))
  | otherwise                   = return $ subst n t


inferLit :: ( MonadState s m, HasInferState s
            , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
            , MonadIO m
            )
         => Lit -> m (Subst, Type)
inferLit = \case
  IntLit _    -> return (nullSubst, TCon . Con $ "Int")
  FloatLit _  -> return (nullSubst, TCon . Con $ "Float")
  CharLit _   -> return (nullSubst, TCon . Con $ "Char")
  BoolLit _   -> return (nullSubst, TCon . Con $ "Bool")


inferInstr :: ( MonadState s m, HasInferState s)
            => PrimInstr -> m (Subst, Type)
inferInstr instr
  | instr `elem` intInstrs = return (nullSubst, tfun2 (tcon_ "Int") (tcon_ "Int") (tcon_ "Int"))
  | instr `elem` floatInstrs = return (nullSubst, tfun2 (tcon_ "Float") (tcon_ "Float") (tcon_ "Float"))
  | otherwise = error "Uknown instruction encountered!" -- Not handle, should be impossible
            

inferExp :: ( MonadState s m, HasInferState s
            , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
            , MonadIO m
            )
         => TypeEnv -> Exp Var -> m (Exp Var, Subst, Type)
inferExp env = \case
  ELoc loc e -> inferExp' env loc e
  _ -> error "Built-in expression encountered!" -- Built-in expressions shouldn't be generated yet

inferExp' :: ( MonadState s m, HasInferState s
            , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
            , MonadIO m
            )
         => TypeEnv -> Location -> Exp Var -> m (Exp Var, Subst, Type)
inferExp' env@(TypeEnv envMap) loc = \case
  ELit lit -> do
    (s, t) <- inferLit lit
    return (EType t $ ELoc loc $ ELit lit, s, t)

  EVar n ->
    case Map.lookup n envMap of
      Nothing -> confessNow (_UnboundVariable # (n, loc))
      Just sigma -> do  t <- instantiate sigma
                        return (EType t $ ELoc loc $ EVar n, nullSubst, t)

  ECon con@(Con n) ->
    case Map.lookup (Var n) envMap of
      Nothing -> confessNow (_UnboundConstructor # (con, loc))
      Just sigma -> do  t <- instantiate sigma
                        return (EType t $ ELoc loc $ ECon con, nullSubst, t)

  EPrim instr -> do
    (s, t) <- inferInstr instr
    return (EType t $ ELoc loc $ EPrim instr, s, t)

  
  EApp a@(ELoc la _) b@(ELoc lb _) -> do
    tv <- newTVar "a"
    (a', s1, t1) <- inferExp env a
    (b', s2, t2) <- inferExp (apply s1 env) b
    s3 <- unify (Just la, Just lb) (apply s2 t1, TFun t2 tv)
    let s' = s3 `composeSubst` s2 `composeSubst` s1
        t' = apply s3 tv
        e' = EType t' $ ELoc loc $ EApp a' b' 
    return (e', s', t')


  ELam n e -> do
    tv <- newTVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv $ env' `Map.union` Map.singleton n (Scheme [] tv)
    (e', s, t) <- inferExp env'' e
    let t' = TFun (apply s tv) t
    return (EType t' $ ELoc loc $ ELam n e', s, t')


  EIf p@(ELoc lp _) e1@(ELoc l1 _) e2@(ELoc l2 _) -> do
    (p', s1, t1) <- inferExp env p
    (e1', s2, t2) <- inferExp env e1
    (e2', s3, t3) <- inferExp env e2
    s4 <- unify (Just lp, Nothing) (t1, TCon . Con $ "Bool")
    s5 <- unify (Just l1, Just l2) (t2, t3)
    let e' = EType t3 $ ELoc loc $ EIf p' e1' e2'
        s' = s5 `composeSubst` s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
    return (e', s', t3)

    
  ELet (n, e1) e2 -> do
    (e1', s1, t1) <- inferExp env e1
    let TypeEnv env' = remove env n
        t' = generalize (apply s1 env) t1
        env'' = TypeEnv (Map.insert n t' env')
    (e2', s2, t2) <- inferExp (apply s1 env'') e2
    let e' = EType t2 $ ELoc loc $ ELet (n, e1') e2'
        s' = s1 `composeSubst` s2
    return (e', s', t2)

  EDup e -> do
    (e', s, t) <- inferExp env e
    return (EType t $ ELoc loc $ EDup e', s, t)

  EFree v e -> do
    (e', s, t) <- inferExp env e
    return (EType t $ ELoc loc $ EFree v e', s, t)

  EType t1 e@(ELoc l1 _) -> do
    (e', s1, t2) <- inferExp env e
    s2 <- unify (Just loc, Just l1) (t1, t2)
    return (ELoc loc $ EType t1 e', s2 `composeSubst` s1, t1)

  e -> error $ "Expression extension is not supported by typechecker.\n" ++ show (pretty e) -- Not handled, should be impossible




infer' :: (MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e, MonadIO m)
       => Map.Map Var Scheme -> Exp Var -> m (Exp Var, Type)
infer' env e = do
  (e', s, t) <- evalInferT . inferExp (TypeEnv env) $ e
  return (e', apply s t)


typecheck :: ( MonadState s m, HasHkcState s
             , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
             , MonadIO m
             ) => m ()
typecheck = do
  env <- use hkcTypes
  defs <- use hkcDefs
  hkcDefs <~ mapM (mapM (checkDef env)) defs

  where
    checkDef env d = do
      r <- memento $ checkDef' env d
      either disclose return r

    checkDef' env (Def (Name n) ps e) = do
      let e' = foldr lamPat e ps
      (e'', t') <- infer' env e'
      hkcTypes . at (Var n) .= Just (Scheme [] t')
      return (Def (Name n) ps e'')

    lamPat (Pat n) = lam_ (Var n)

