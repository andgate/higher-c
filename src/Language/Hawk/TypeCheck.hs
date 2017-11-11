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
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control

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

import Language.Hawk.Syntax
import Language.Hawk.TypeCheck.Assumption (Assumption)
import Language.Hawk.TypeCheck.Environment (Env)
import Language.Hawk.TypeCheck.Error
import Language.Hawk.TypeCheck.Message
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


class Substitutable a where
  apply :: Subst -> a -> a


instance Substitutable Text where
  apply (Subst s) a = tv
    where t = TVar a
          (TVar tv) = Map.findWithDefault t a s

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


instance FreeTypeVars Text where
  ftv = Set.singleton


instance FreeTypeVars Scheme where
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as


instance FreeTypeVars a => FreeTypeVars [a] where
  ftv = foldr (Set.union . ftv) Set.empty


instance (Ord a, FreeTypeVars a) => FreeTypeVars (Set a) where
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
runInfer :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
            , MonadChronicle (Bag e) m, AsTcErr e
            )
         => ReaderT (Set Text) (StateT InferState m) a -> m a
runInfer m = evalStateT (runReaderT m Set.empty) initInfer



inferType :: ( MonadReader (Set Text) m
             , MonadState s m, HasInferState s
             , MonadLog (WithSeverity msg) m, AsTcMsg msg
             , MonadChronicle (Bag e) m, AsTcErr e
             )
          => Env -> Exp -> m (Subst, Type)
inferType env ex = do
  (as, cs, t) <- infer ex
  let unbounds = Set.fromList (As.keys as) `Set.difference` Set.fromList (Env.keys env)
  unless (Set.null unbounds)
         $ disclose $ One (_UnboundVariable # Set.findMin unbounds)
  let cs' = [ExpInstConst t s | (x, s) <- Env.toList env, t <- As.lookup x as]

  subst <- solve (cs ++ cs')
  return (subst, apply subst t)


-- | Solve for the toplevel type of an expression
inferExp :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
            , MonadChronicle (Bag e) m, AsTcErr e
            )
         => Env -> Exp -> m Scheme
inferExp env ex = do
  (subst, ty) <- runInfer (inferType env ex)
  return $ closeOver $ apply subst ty
  


-- | Cannonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize Set.empty


extendMSet :: MonadReader (Set Text) m
           => Text -> m a -> m a
extendMSet x = local (Set.insert x)


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

genftv :: Int -> Type
genftv n =
  TVar $ pack (letters !! n)

fresh :: (MonadState s m, HasInferState s)
      => m Type
fresh = do
  countfv += 1
  uses countfv genftv


instantiate :: (MonadState s m, HasInferState s)
            => Scheme -> m Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s t


generalize :: Set Text -> Type -> Scheme
generalize free t = Forall as t
  where as = Set.toList $ ftv t `Set.difference` free


normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map (pack . snd) ord) (normtype body)
  where
    ord = zip (nub $ fv body) letters

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _)   = []


    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a) = TCon a
    normtype (TVar a) =
      case Prelude.lookup a ord of
        Just x -> TVar (pack x)
        Nothing -> error "type variable not in signature."

--ops :: Operator -> Type


inferLit :: Lit -> Type
inferLit = \case
  IntLit _   -> TCon "Int"
  FloatLit _ -> TCon "Float"
  CharLit _  -> TCon "Char"
  BoolLit _  -> TCon "Bool"


inferPrimInstr :: PrimInstr -> Type
inferPrimInstr = \case
  PrimAdd  -> tFun2 tInt tInt tInt
  PrimFAdd -> tFun2 tFloat tFloat tFloat
  PrimSub  -> tFun2 tInt tInt tInt
  PrimFSub -> tFun2 tFloat tFloat tFloat
  PrimMul  -> tFun2 tInt tInt tInt
  PrimFMul -> tFun2 tFloat tFloat tFloat
  PrimDiv  -> tFun2 tInt tInt tInt
  PrimUDiv -> error "Unsigned division not implemented in typechecker."
  PrimSDiv -> error "Short division not implemented in typechecker."
  PrimFDiv -> tFun2 tFloat tFloat tFloat
  PrimBad  -> error "Type checker encountered bad primitive instruction."

infer :: ( MonadReader (Set Text) m
         , MonadState s m, HasInferState s
         , MonadLog (WithSeverity msg) m, AsTcMsg msg
         , MonadChronicle (Bag e) m, AsTcErr e
         )
      => Exp -> m (Assumption, [Constraint], Type)
infer = \case
  EVar x -> do
    tv <- fresh
    return (As.singleton x tv, [], tv)


  EApp e1 e2 -> do
    (as1, cs1, t1) <- infer e1
    (as2, cs2, t2) <- infer e2
    tv <- fresh
    return ( as1 `As.merge` as2
           , cs1 ++ cs2 ++ [EqConst t1 (t2 `TArr` tv)]
           , tv
           )


  ELam x e -> do
    tv@(TVar a) <- fresh
    (as, cs, t) <- extendMSet a (infer e)
    return ( as `As.remove` x
           , cs ++ [EqConst t' tv | t' <- As.lookup x as]
           , tv `TArr` t
           )
    

  ELet (x, e1) e2 -> do
    (as1, cs1, t1) <- infer e1
    (as2, cs2, t2) <- infer e2
    ms <- ask
    return ( as1 `As.merge` as2 `As.remove` x
           , cs1 ++ cs2 ++ [ImpInstConst t' ms t1 | t' <- As.lookup x as2]
           , t2
           )


  ELit l ->
    let t = inferLit l
    in return (As.empty, [], t)
    -- Maybe in the future, literals can take on multiple possible
    -- types through constraints? So '32' can be use as a double or int.


  ECon n -> do
    tv <- fresh
    -- Should this try to find the constructors type??
    return (As.singleton n tv, [], tv)

  EPrim i ->
    let t = inferPrimInstr i
    in return (As.empty, [], t)

  EIf e1 e2 e3 -> do
    (as1, cs1, t1) <- infer e1
    (as2, cs2, t2) <- infer e2
    (as3, cs3, t3) <- infer e3
    return ( as1 `As.merge` as2 `As.merge` as3
           , cs1 ++ cs2 ++ cs3 ++ [EqConst t1 tBool, EqConst t2 t3]
           , t2
           )

  EDup e -> infer e

  EFree n e -> infer e

  EType t e -> do
    (as, cs, t') <- infer e
    return (as, cs ++ [EqConst t t'], t)

  ETLit tlit e -> error "Type inferenece doesn't work on type literals"

  ELoc l e -> infer e

  EParen e -> infer e

inferTop :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
            , MonadChronicle (Bag e) m, AsTcErr e )
         => Env -> [(Text, Exp)] -> m Env
inferTop env [] = return env
inferTop env ((name, ex):xs) = do
  ty <- inferExp env ex
  inferTop (Env.extend env (name, ty)) xs


typecheck :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
             , MonadChronicle (Bag e) m, AsTcErr e )
          => Map FilePath [Decl] -> m (Map FilePath [Decl])
typecheck ds = do
  let exSig (Sig n ty) ns = (n, Forall [] ty):ns
      exSig _          ns = ns

      exExp (Def n e) es = (n, e):es
      exExp _         es = es

      ts = foldr exSig [] $ concat $ Map.elems ds
      es = foldr exExp [] $ concat $ Map.elems ds
      env = Env.fromList ts
      
  env' <- inferTop env es
  logInfo (_TcComplete # ())
  return ds


-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------


-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty


-- | Compose substitutions
compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) =
  Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1


unifyMany :: (MonadChronicle (Bag e) m, AsTcErr e)
          => [Type] -> [Type] -> m Subst
unifyMany [] [] = return emptySubst
unifyMany (t1:ts1) (t2:ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return $ su2 `compose` su1

unifyMany t1 t2 = disclose $ One (_UnificationMismatch # (t1, t2))


unifies :: (MonadChronicle (Bag e) m, AsTcErr e)
        => Type -> Type -> m Subst
unifies t1 t2
  | t1 == t2 = return emptySubst
  | otherwise = case (t1, t2) of
      -- Unify variables
      (TVar v, t) -> v `bind` t
      (t, TVar v) -> v `bind` t

      -- Unify arrows
      (TArr t1 t2, TArr t3 t4) -> unifyMany [t1,t2] [t3,t4]
      (TLoli t1 t2, TLoli t3 t4) -> unifyMany [t1,t2] [t3,t4]

      -- Unify kinded types
      -- This should probably test kinds too
      (TKind k t1, t2) -> unifies t1 t2
      (t1, TKind k t2) -> unifies t1 t2

      -- Unify located types
      (TLoc l t1, t2) -> unifies t1 t2
      (t1, TLoc l t2) -> unifies t1 t2

      -- Unify types in parens
      (TParen t1, t2) -> unifies t1 t2
      (t1, TParen t2) -> unifies t1 t2

      (t1, t2) -> disclose $ One (_UnificationFail # (t1, t2))


bind :: (MonadChronicle (Bag e) m, AsTcErr e)
     => Text -> Type -> m Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = disclose $ One (_InfiniteType # (a, t))
         | otherwise       = return (Subst $ Map.singleton a t)
     

occursCheck :: FreeTypeVars a => Text -> a -> Bool
occursCheck a t = a `Set.member` ftv t


nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs = fromJust (find solvable (chooseOne xs))
  where
    chooseOne xs = [(x, ys) | x <- xs, let ys = delete x xs]
    solvable (EqConst{}, _)      = True
    solvable (ExpInstConst{}, _) = True
    solvable (ImpInstConst ts ms t2, cs) = Set.null ((ftv t2 `Set.difference` ms) `Set.intersection` atv cs)


solve :: ( MonadState s m, HasInferState s
         , MonadChronicle (Bag e) m, AsTcErr e
         )
      => [Constraint] -> m Subst
solve [] = return emptySubst
solve cs = solve' (nextSolvable cs)


solve' :: ( MonadState s m, HasInferState s
          , MonadChronicle (Bag e) m, AsTcErr e
          )
       => (Constraint, [Constraint]) -> m Subst
solve' = \case
  (EqConst t1 t2, cs) -> do
    su1 <- unifies t1 t2
    su2 <- solve (apply su1 cs)
    return (su2 `compose` su1)

  (ImpInstConst t1 ms t2, cs) ->
    solve (ExpInstConst t1 (generalize ms t2) : cs)

  (ExpInstConst t s, cs) -> do
    s' <- instantiate s
    solve (EqConst t s' : cs)
