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
import Language.Hawk.Syntax
import Language.Hawk.Syntax.Prim
import Language.Hawk.TypeCheck.Error

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T



newtype TypeEnv = TypeEnv (Map Var Scheme)
newtype Subst = Subst { substDict :: Map Tyvar Type }
  deriving (Eq, Ord, Show)

data Constraint
  = EqConst Type Type
  | ImpInstConst Type (Set Tyvar) Type
  | ExpInstConst Type Scheme

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
newtype InferT m a = InferT { unInferT :: ReaderT (Set Tyvar) (StateT InferState m) a }
  deriving (Functor, Applicative, Monad, MonadReader (Set Tyvar), MonadState InferState)

runInferT :: Monad m => InferT m a -> m a
runInferT m = evalStateT (runReaderT (unInferT m) Set.empty) def

newTVar :: (MonadState s m, HasInferState s)
        => Text -> m Type 
newTVar prefix = do
  i <- pack . show <$> use inferSupply
  inferSupply += 1
  return . TVar . Tyvar $ prefix `T.append` i  

deriving instance MonadIO m => MonadIO (InferT m)

instance MonadTrans InferT where
    lift = InferT . lift . lift

instance MonadChronicle c m => MonadChronicle c (InferT m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (InferT m) = InferT $ memento m
    absolve x (InferT m) = InferT $ absolve x m
    condemn (InferT m) = InferT $ condemn m
    retcon f (InferT m) = InferT $ retcon f m
    chronicle = lift . chronicle


-- Free Type Variables
class FreeTVars a where
    ftv :: a -> Set Tyvar

instance FreeTVars Type where
    ftv = \case
      TCon _    -> Set.empty
      TVar v    -> Set.singleton v
      TApp a b  -> ftv a `Set.union` ftv b

instance FreeTVars Tyvar where
    ftv = Set.singleton

instance FreeTVars Scheme where
    ftv (Forall tvs t) = ftv t \\ Set.fromList tvs

instance FreeTVars a => FreeTVars [a] where
    ftv = foldr (Set.union . ftv) Set.empty

instance (Ord a, FreeTVars a) => FreeTVars (Set a) where
    ftv = foldr (Set.union . ftv) Set.empty

-- Active Type Variables
class ActiveTVars a where
    atv :: a -> Set Tyvar

instance ActiveTVars Constraint where
    atv = \case
      EqConst t1 t2         -> ftv t1 `Set.union` ftv t2
      ImpInstConst t1 ms t2 -> ftv t1 `Set.union` (ftv ms `Set.intersection` ftv t2)
      ExpInstConst t s      -> ftv t `Set.union` ftv s

-- Substituble
class Substitutable a where
    apply :: Subst -> a -> a

instance Substitutable Tyvar where
    apply (Subst s) a = tv
      where t = TVar a
            (TVar tv) = Map.findWithDefault t a s

instance Substitutable Type where
    apply s = \case
      t@(TVar a)  -> Map.findWithDefault t a (substDict s)
      TCon a      -> TCon a
      TApp a b    -> TApp (apply s a) (apply s b)


instance Substitutable Scheme where
    apply (Subst s) (Forall as t) = Forall as $ apply s' t
                      where s' = Subst $ foldr Map.delete s as


instance Substitutable a => Substitutable [a] where
    apply = map . apply

instance (Ord a, Substitutable a) => Substitutable (Set a) where
    apply = Set.map . apply

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)


-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------


typecheck :: ( MonadState s m, HasHkcState s
             , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
             , MonadIO m
             ) => m ()
typecheck = do
  env <- use hkcTypes
  defs <- use hkcDefs
  hkcDefs <~ mapM (mapM (conceal . checkDef env)) defs

  where
    checkDef env (Def (Name n) ps e) = do
      let e' = foldr lamPat e ps
      (e'', t') <- infer' env e'
      hkcTypes . at (Var n) .= Just (Forall [] t')
      return (Def (Name n) ps e'')

    lamPat (Pat n) = lam_ (Var n)


infer' :: (MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e, MonadIO m)
       => Map.Map Var Scheme -> Exp Var -> m (Exp Var, Type)
infer' env e = do
  (e', s, t) <- runInferT . inferExp (TypeEnv env) $ e
  return (e', apply s t)



closeOver :: Type -> Scheme
closeOver = normalize . generalize Set.empty

extendMSet :: MonadReader (Set Tyvar) m => Tyvar -> m a -> m a
extendMSet x = local (Set.insert x)

letters :: [String]
letters = [1..] >>= flip replicateM ['a' .. 'z']


fresh :: (MonadState s m, HasInferState s) => m Type
fresh = do
  inferSupply += 1
  uses inferSupply $
    TVar . Tyvar . pack . (letters !!)


-- The core of the Algorithm W
instantiate :: (MonadState s m, HasInferState s)
            => Scheme -> m Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst . Map.fromList $ zip as as'
  return $ apply s t



generalize :: Set Tyvar -> Type -> Scheme
generalize free t = Forall as t
  where as = Set.toList $ ftv t \\ free


inferLit :: ( MonadState s m, HasInferState s
            , MonadChronicle (Bag (WithTimestamp e)) m, AsTcErr e
            , MonadIO m
            )
         => Lit -> m (Subst, Type)
inferLit = \case
  IntLit _    -> return (nullSubst, TCon . Tycon $ "Int")
  FloatLit _  -> return (nullSubst, TCon . Tycon $ "Float")
  CharLit _   -> return (nullSubst, TCon . Tycon $ "Char")
  BoolLit _   -> return (nullSubst, TCon . Tycon $ "Bool")


inferInstr :: ( MonadState s m, HasInferState s)
            => PrimInstr -> m (Subst, Type)
inferInstr instr
  | instr `elem` intInstrs = return (nullSubst, tFun2 (tcon_ "Int") (tcon_ "Int") (tcon_ "Int"))
  | instr `elem` floatInstrs = return (nullSubst, tFun2 (tcon_ "Float") (tcon_ "Float") (tcon_ "Float"))
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
    s3 <- unify (Just la, Just lb) (apply s2 t1, TApp t2 tv)
    let s' = s3 `composeSubst` s2 `composeSubst` s1
        t' = apply s3 tv
        e' = EType t' $ ELoc loc $ EApp a' b' 
    return (e', s', t')


  ELam n e -> do
    tv <- newTVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv $ env' `Map.union` Map.singleton n (Forall [] tv)
    (e', s, t) <- inferExp env'' e
    let t' = TApp (apply s tv) t
    return (EType t' $ ELoc loc $ ELam n e', s, t')


  EIf p@(ELoc lp _) e1@(ELoc l1 _) e2@(ELoc l2 _) -> do
    (p', s1, t1) <- inferExp env p
    (e1', s2, t2) <- inferExp env e1
    (e2', s3, t3) <- inferExp env e2
    s4 <- unify (Just lp, Nothing) (t1, TCon . Tycon $ "Bool")
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


normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map (Tyvar . pack) letters)

    fv = \case
      TVar a    -> [a]
      TCon _    -> []
      TApp a b  -> fv a ++ fv b
      TKind _ t -> fv t

    normtype = \case
      TApp a b  -> TApp (normtype a) (normtype b)
      TCon a    -> TCon a
      TKind k t -> TKind k t
      TVar a    -> case Prelude.lookup a ord of
                     Just x   -> TVar x
                     Nothing  -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- Substitution Helpers
nullSubst :: Subst
nullSubst = Subst Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst subst1@(Subst s1) (Subst s2)
  = Subst $ Map.map (apply subst1) s2 `Map.union` s1

instance Monoid Subst where
  mempty = nullSubst
  mappend = composeSubst

instance Default Subst where
    def = nullSubst


unifyMany :: ([Type], [Type]) -> m Subst
unifyMany = \case
  ([], []) -> return mempty
  (t1:ts1, t2:ts2) ->
    do  su1 <- unifies t1 t2
        su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
        return $ su1 <> su2
  (t1, t2) -> error "Unification mismatch"


unifies :: (Type, Type) -> m Subst
unifies = \case
  (t1, t2)
    | t1 == t2 -> return mempty
  
  (TVar v, t) -> v `bind` t
  (t, TVar v) -> v `bind` t

  (TApp a1 b1, TApp a2 b2) -> unifyMany [a1, b1] [a2, b2]

  _ -> error "Unification Fail"


bind :: Tyvar -> Type -> m Subst
bind a t
  | t == TVar a = return mempty
  | occursCheck a t = error "Infinite Type Error"
  | otherwise = return . Subst $ Map.singleton a t


occursCheck :: FreeTVars a => Tyvar -> a -> Bool
occursCheck a t = a `Set.member` ftv t


nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs = fromJust (find solvable (chooseOne xs))
  where
    chooseOne xs = [(x, ys) | x <- xs, let ys = delete x xs]

    solvable = \case
      (EqConst{}, _)      -> True
      (ExpInstConst{}, _) -> True
      (ImpInstConst t1 ms t2, cs) -> Set.null ((ftv t2 `Set.difference` ms) `Set.intersection` atv cs)


solve :: [Constraint] -> m Subst
solve [] = return mempty
solve cs = solve' . nextSolvable $ cs

solve' :: (Constraint, [Constraint]) -> m Subst
solve' = \case
  (EqConst t1 t2, cs) ->
    do  su1 <- unifies t1 t2
        su2 <- solve $ apply su1 cs
        return $ su2 <> su1

  (ImpInstConst t1 ms t2, cs) -> solve $ ExpInstConst t1 (generalize ms t2):cs

  (ExpInstConst t s, cs) ->
    do  s' <- instantiate s
        solve $ EqConst t s' : cs