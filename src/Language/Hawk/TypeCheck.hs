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
import Language.Hawk.TypeCheck.Substitution (Subst(..))


import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T
import qualified Language.Hawk.TypeCheck.Assumption as As
import qualified Language.Hawk.TypeCheck.Environment as Env
import qualified Language.Hawk.TypeCheck.Substitution as Subst



-----------------------------------------------------------------------
-- Free Type Variables
-----------------------------------------------------------------------

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


-----------------------------------------------------------------------
-- Active Type Variables
-----------------------------------------------------------------------

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
-- Constraint Generation
-----------------------------------------------------------------------

data TypeResult
  = TypeResult
      { constraints :: [Constraint]
      , assumptions :: Assumption
      } deriving(Show)


instance Monoid TypeResult where
  mempty = TypeResult
           { constraints = mempty
           , assumptions = As.empty
           }

  mappend (TypeResult cs1 as1) (TypeResult cs2 as2)
    = TypeResult
        { constraints = cs1 `mappend` cs2
        , assumptions = as1 `As.merge` as2
        }


-- | Run the inference monad
runInfer :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
            , MonadChronicle (Bag e) m, AsTcErr e
            )
         => ReaderT (Set Text) (StateT TypeState m) a -> m a
runInfer m = evalStateT (runReaderT m Set.empty) def



inferType :: ( MonadReader (Set Text) m
             , MonadState s m, HasTypeState s
             , MonadLog (WithSeverity msg) m, AsTcMsg msg
             , MonadChronicle (Bag e) m, AsTcErr e
             )
          => Env -> Exp -> m (Subst, Type, Exp)
inferType env ex = do
  (t, e', TypeResult cs as) <- generateConstraints ex
  let unbounds = Set.fromList (As.keys as) `Set.difference` Set.fromList (Env.keys env)
  unless (Set.null unbounds)
         $ disclose $ One (_UnboundVariable # Set.findMin unbounds)
  let cs' = [ ExpInstConst t s | (x, s) <- Env.toList env
                               , t <- As.lookup x as ]

  s <- solve (cs ++ cs')
  return (s, Subst.apply s t, Subst.apply s e')


-- | Solve for the toplevel type of an expression
inferExp :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
            , MonadChronicle (Bag e) m, AsTcErr e
            )
         => Env -> Exp -> m (Exp, Scheme)
inferExp env e = do
  (s, t, e') <- runInfer (inferType env e)
  return (Subst.apply s e', closeOver $ Subst.apply s t)
  


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


fresh :: (MonadState s m, HasTypeState s)
      => m Type
fresh = do
  countfv += 1
  uses countfv genftv


instantiate :: (MonadState s m, HasTypeState s)
            => Scheme -> m Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ Subst.apply s t


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


generateConstraints
  :: ( MonadReader (Set Text) m
     , MonadState s m, HasTypeState s
     , MonadLog (WithSeverity msg) m, AsTcMsg msg
     , MonadChronicle (Bag e) m, AsTcErr e )
  => Exp -> m (Type, Exp, TypeResult)
generateConstraints = \case
  EVar x -> do
    tv <- fresh
    return ( tv
           , EType tv $ EVar x
           , TypeResult { constraints = []
                        , assumptions = As.singleton x tv })


  EApp e1 e2 -> do
    tv <- fresh
    (t1, e1', r1) <- generateConstraints e1
    (t2, e2', r2) <- generateConstraints e2
    return ( tv
           , EType tv $ EApp e1' e2'
           , r1 <> r2 <> TypeResult { constraints = [EqConst t1 (t2 `TArr` tv)]
                                    , assumptions = As.empty }
           )


  ELam x e -> do
    tv@(TVar a) <- fresh
    (t, e', TypeResult cs as) <- extendMSet a $ generateConstraints e
    let as' = as `As.remove` x 
    return ( tv `TArr` t
           , EType (tv `TArr` t) $ ELam x e'
           , TypeResult { constraints = cs ++ [EqConst t' tv | t' <- As.lookup x as]
                        , assumptions = as' }
           )
    

  ELet (x, e1) e2 -> do
    (t1, e1', TypeResult cs1 as1) <- generateConstraints e1
    (t2, e2', TypeResult cs2 as2) <- generateConstraints e2
    ms <- ask
    return ( t2
           , EType t2 $ ELet (x, e1') e2'
           , TypeResult { constraints = cs1 ++ cs2 ++ [ImpInstConst t' ms t1 | t' <- As.lookup x as2]
                        , assumptions = as1 `As.merge` as2 `As.remove` x }
           )


  ELit l ->
    let t = inferLit l
    in return ( t
              , EType t $ ELit l
              , mempty)
    -- Maybe in the future, literals can take on multiple possible
    -- types through constraints? So '32' can be use as a double or int.


  ECon n -> do
    tv <- fresh
    -- Should look up constructors type here?
    return ( tv
           , EType tv $ ECon n
           , TypeResult
             { constraints = []
             , assumptions = As.singleton n tv })

  EPrim i ->
    let t = inferPrimInstr i
    in return ( t
              , EType t $ EPrim i
              , mempty)

  EIf e1 e2 e3 -> do
    (t1, e1', r1) <- generateConstraints e1
    (t2, e2', r2) <- generateConstraints e2
    (t3, e3', r3) <- generateConstraints e3
    return ( t2
           , EType t2 $ EIf e1' e2' e3'
           , r1 <> r2 <> r2
                <> TypeResult [EqConst t1 tBool, EqConst t2 t3] As.empty
           )

  EDup e -> do
    (t, e', r) <- generateConstraints e
    return ( t
           , EType t $ EDup e'
           , r )

  EFree n e -> do
    (t, e', r) <- generateConstraints e
    return ( t
           , EType t $ EFree n e'
           , r)

  EType t e -> do
    (t', e', r) <- generateConstraints e
    return ( t
           , EType t e
           , r <> TypeResult [EqConst t t'] As.empty)

  ETLit tlit e -> error "Type inferenece doesn't work on type literals"

  ELoc l e -> do
    (t, e', r) <- generateConstraints e
    return (t, ELoc l e', r)

  EParen e -> do
    (t, e', r) <- generateConstraints e
    return ( t
           , EType t $ EParen e'
           , r)



inferTop :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
            , MonadChronicle (Bag e) m, AsTcErr e )
         => Env -> [(Text, Exp)] -> m ([(Text, Exp)], Env)
-- Base case takes an environment and produces a map of text to exp, type
-- Recursive part continuously 
inferTop env = do
  foldM (\(es, env') (n, e) -> do
            (e', ty) <- inferExp env e
            let env' = Env.extend env (n, ty)
            return ((n, e'):es, env')
        ) ([], env)


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
      
  (es, env') <- inferTop env es
  logInfo (_TcComplete # ())
  return ds


-------------------------------------------------------------------------------
-- Constraint Solving
-------------------------------------------------------------------------------




unifyMany :: (MonadChronicle (Bag e) m, AsTcErr e)
          => [Type] -> [Type] -> m Subst
unifyMany [] [] = return Subst.empty
unifyMany (t1:ts1) (t2:ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (Subst.apply su1 ts1) (Subst.apply su1 ts2)
     return $ su2 `Subst.compose` su1

unifyMany t1 t2 = disclose $ One (_UnificationMismatch # (t1, t2))


unifies :: (MonadChronicle (Bag e) m, AsTcErr e)
        => Type -> Type -> m Subst
unifies t1 t2
  | t1 == t2 = return Subst.empty
  | otherwise = case (t1, t2) of
      -- Unify variables
      (TVar v, t) -> v `bind` t
      (t, TVar v) -> v `bind` t

      -- Unify arrows
      (TArr t1 t2, TArr t3 t4) -> unifyMany [t1,t2] [t3,t4]
      (TLoli t1 t2, TLoli t3 t4) -> unifyMany [t1,t2] [t3,t4]

      -- Unify kinded types
      -- This should probably test kinds too
      ts@(TKind k1 t1, TKind k2 t2) -> do
        unless (k1 `ksub` kind t2)
               (confess $ One (_UnificationFail # ts))
        unifies t1 t2

      -- Unify located types
      (t1, TLoc _ t2) -> unifies t1 t2
      (TLoc _ t1, t2) -> unifies t1 t2

      -- Unify types in parens
      (TParen t1, t2) -> unifies t1 t2
      (t1, TParen t2) -> unifies t1 t2

      (t1, t2) -> confess $ One (_UnificationFail # (t1, t2))


bind :: (MonadChronicle (Bag e) m, AsTcErr e)
     => Text -> Type -> m Subst
bind a t | t == TVar a     = return Subst.empty
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


solve :: ( MonadState s m, HasTypeState s
         , MonadChronicle (Bag e) m, AsTcErr e
         )
      => [Constraint] -> m Subst
solve [] = return Subst.empty
solve cs = solve' (nextSolvable cs)


solve' :: ( MonadState s m, HasTypeState s
          , MonadChronicle (Bag e) m, AsTcErr e
          )
       => (Constraint, [Constraint]) -> m Subst
solve' = \case
  -- Equality Constraint
  (EqConst t1 t2, cs) -> do
    -- Unify t1 with t2.
    su1 <- unifies t1 t2
    su2 <- solve (Subst.apply su1 cs)
    return (su2 `Subst.compose` su1)

  -- Implicit Instance Constraint
  (ImpInstConst t1 ms t2, cs) ->
    -- Generalize t2 with tvar set ms, and
    -- add new constraint equating the two.
    solve (ExpInstConst t1 (generalize ms t2) : cs)

  -- Explicit Instance Constraint
  (ExpInstConst t s, cs) -> do
    -- Instantiate the given scheme to a type.
    t' <- instantiate s
    -- Unify the instantiated scheme with type t.
    solve (EqConst t t' : cs)
