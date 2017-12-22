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
import Control.Arrow (second)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra (mconcatMapM)
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control

import Data.Bag
import Data.Default.Class
import Data.List (lookup, union, concatMap, nub, find, delete, intersect)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding ((<>))
import Data.Set (Set, (\\))
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Syntax
import Language.Hawk.TypeCheck.Assumption (Assumption)
import Language.Hawk.TypeCheck.Constraint
import Language.Hawk.TypeCheck.Environment (Env)
import Language.Hawk.TypeCheck.Error
import Language.Hawk.TypeCheck.Message
import Language.Hawk.TypeCheck.State
import Language.Hawk.TypeCheck.Substitution (Subst(..))


import qualified Data.Map.Strict as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T
import qualified Language.Hawk.TypeCheck.Assumption as As
import qualified Language.Hawk.TypeCheck.Environment as Env
import qualified Language.Hawk.TypeCheck.Substitution as Subst


-----------------------------------------------------------------------
-- Type Checking
-----------------------------------------------------------------------

typecheck :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
                    , MonadChronicle (Bag e) m, AsTcErr e )
                 => Image -> m Image
typecheck img = evalStateT m $ Env.fromImg img
  where
    m = do
      logInfo (_TcBegin # ())
      img' <- condemn $ img & mapMOf (imgFns.each) (typecheckWith typecheckFn)
      logInfo (_TcComplete # ())
      return img'


typecheckWith
  :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
     , MonadChronicle (Bag e) m, AsTcErr e 
     )
  => (Env -> a -> m (b, Env)) -> a -> StateT Env m b
typecheckWith tc a = do
  env <- get
  (a', env') <- lift $ tc env a
  put env'
  return a'

  
typecheckFn
  :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
     , MonadChronicle (Bag e) m, AsTcErr e
     )
  => Env -> Fn -> m (Fn, Env)
typecheckFn env f = run $ do
  (t, f', InferResult cs as) <- inferFn env f
  
  let unbounds = Set.fromList (As.keys as) `Set.difference` Set.fromList (Env.typeNames env)
  unless (Set.null unbounds)
         $ disclose $ One (_UnboundVariables # Set.toList unbounds)
  let cs' = [ ExpInstConst t s | (x, s) <- Env.toTypes env
                               , t <- As.lookup x as ]
  
  s <- solve (cs ++ cs')
  let f'' = Subst.apply s f'
      t'  = closeOver $ Subst.apply s t
      env' = Env.extendType env (readName . _fnName $ f, t')
  return (f'', env')
  
  where
    run m = evalStateT (runReaderT m Set.empty) (def :: TypeState)
        
        
-----------------------------------------------------------------------
-- Constraint Inference
-----------------------------------------------------------------------

data InferResult
  = InferResult
      { constraints :: [Constraint]
      , assumptions :: Assumption
      }
    deriving(Show)


instance Monoid InferResult where
  mempty = InferResult
    { constraints = mempty
    , assumptions = As.empty
    }

  mappend (InferResult cs1 as1) (InferResult cs2 as2)
    = InferResult
        { constraints = cs1 `mappend` cs2
        , assumptions = as1 `As.merge` as2
        }

instance Default InferResult where
  def = mempty



inferFn
  :: ( MonadReader (Set Text) m
    , MonadState s m, HasTypeState s
    , MonadLog (WithSeverity msg) m, AsTcMsg msg
    , MonadChronicle (Bag e) m, AsTcErr e )
  => Env -> Fn -> m (Type, Fn, InferResult)
inferFn env (Fn n ps e) = do
  let ns = concatMap patNames ps
  tvs <- mapM (const fresh) ns
  let tvns = Set.toList $ ftv tvs

  (ts, ps', InferResult cs1 as1) <- inferPatterns env ps
  (t, e', InferResult cs2 as2) <- extendsMSet tvns $ inferExp (env, locExp e) e
  
  let as = as1 `As.merge` as2 --`As.removeMany` ns  -- remove names from assumptions?
      cs3 = concatMap (\(x, tv) -> [EqConst y tv | y <- As.lookup x as]) (zip ns tvs)
      cs = cs1 ++ cs2 ++ cs3
      t' = foldr TArr t ts

  return ( t'
          , Fn n ps' e'
          , InferResult { constraints = cs
                        , assumptions = as `As.removeMany` ns }
          )


inferExp
  :: ( MonadReader (Set Text) m
     , MonadState s m, HasTypeState s
     , MonadLog (WithSeverity msg) m, AsTcMsg msg
     , MonadChronicle (Bag e) m, AsTcErr e )
  => (Env, Loc) -> Exp -> m (Type, Exp, InferResult)
inferExp s@(env, l) = \case
  EVar x -> do
    tv <- fresh
    return ( tv
           , EType tv $ EVar x
           , InferResult { constraints = []
                         , assumptions = As.singleton x tv })


  EApp e1 e2 -> do
    tv <- fresh
    (t1, e1', r1) <- inferExp s e1
    (t2, e2', r2) <- inferExp s e2
    return ( tv
           , EType tv $ EApp e1' e2'
           , r1 <> r2 <> InferResult { constraints = [EqConst t1 (t2 `TArr` tv)]
                                     , assumptions = As.empty }
           )


  ELam n e -> do
    tv@(TVar a) <- fresh
    (t, e', InferResult cs as) <- extendMSet a $ inferExp s e

    let x = readName n
        t' = tv `TArr` t

    return ( t'
           , EType t' $ ELam n e'
           , InferResult { constraints = cs ++ [EqConst y tv | y <- As.lookup x as]
                         , assumptions = as `As.remove` x }
           )
    

  ELet (n, e1) e2 -> do
    let x = readName n
    (t1, e1', InferResult cs1 as1) <- inferExp s e1
    (t2, e2', InferResult cs2 as2) <- inferExp s e2
    ms <- ask
    return ( t2
           , EType t2 $ ELet (n, e1') e2'
           , InferResult { constraints = cs1 ++ cs2 ++ [ImpInstConst t' ms t1 | t' <- As.lookup x as2]
                         , assumptions = as1 `As.merge` as2 `As.remove` x }
           )


  ELit l ->
    let t = inferLit l
    in return ( t
              , EType t $ ELit l
              , mempty)


  ECon n ->
    case Env.lookupType n env of
      Just t ->
        return ( t
              , EType t $ ECon n
              , mempty )
      
      Nothing -> disclose $ One (_UndefinedConstructor # (n, l))

  EPrim i e1 e2 -> do
    let t = inferPrimInstr i
    tv <- fresh
    (t1, e1', r1) <- inferExp s e1
    (t2, e2', r2) <- inferExp s e2
    return ( t
             , EType t $ EPrim i e1' e2'
             , r1 <> r2 <> InferResult { constraints = [EqConst t (tFun2 t1 t2 tv)]
                                       , assumptions = As.empty })

  EIf e1 e2 e3 -> do
    (t1, e1', r1) <- inferExp s e1
    (t2, e2', r2) <- inferExp s e2
    (t3, e3', r3) <- inferExp s e3
    return ( t2
           , EType t2 $ EIf e1' e2' e3'
           , r1 <> r2 <> r3
                <> InferResult [EqConst t1 tBool, EqConst t2 t3] As.empty
           )

  EDup n -> do
    tv <- fresh    
    return ( tv
           , EType tv $ EDup n
           , InferResult { constraints = []
                         , assumptions = As.singleton (readName n) tv } )

  EFree n e -> do
    (t, e', r) <- inferExp s e
    return ( t
           , EType t $ EFree n e'
           , r)

  EType t e -> do
    (t', _, r) <- inferExp s e
    return ( t
           , EType t e
           , r <> InferResult [EqConst t t'] As.empty)

  ELoc l' e -> do
    (t, e', r) <- inferExp (env, l') e
    return (t, ELoc l' e', r)

  EParen e -> do
    (t, e', r) <- inferExp s e
    return ( t
           , EType t $ EParen e'
           , r)

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
  
  PrimEq     -> tFun2 tInt tInt tBool
  PrimLt     -> tFun2 tInt tInt tBool
  PrimLtEq   -> tFun2 tInt tInt tBool
  PrimGt     -> tFun2 tInt tInt tBool
  PrimtGtEq  -> tFun2 tInt tInt tBool
  PrimNEq    -> tFun2 tInt tInt tBool
  PrimNLt    -> tFun2 tInt tInt tBool
  PrimNLtEq  -> tFun2 tInt tInt tBool
  PrimNGt    -> tFun2 tInt tInt tBool
  PrimNGtEq  -> tFun2 tInt tInt tBool
  
  PrimBad  -> error "Type checker encountered bad primitive instruction."


inferPatterns
  :: ( MonadReader (Set Text) m
     , MonadState s m, HasTypeState s
     , MonadLog (WithSeverity msg) m, AsTcMsg msg
     , MonadChronicle (Bag e) m, AsTcErr e )
  => Env -> [Pat] -> m ([Type], [Pat], InferResult)
inferPatterns env ps = do
  rs <- mapM (\p -> inferPattern (env, locPat p) p) ps 
  let ts' = (^._1) <$> rs
      ps' = (^._2) <$> rs
      r = mconcat $ (^._3) <$> rs
  return (ts', ps', r)

inferPattern
  :: ( MonadReader (Set Text) m
     , MonadState s m, HasTypeState s
     , MonadLog (WithSeverity msg) m, AsTcMsg msg
     , MonadChronicle (Bag e) m, AsTcErr e )
  => (Env, Loc) -> Pat -> m (Type, Pat, InferResult)
inferPattern s@(env, l) = \case
  PVar x -> do
    tv <- fresh
    return ( tv
           , PType tv $ PVar x
           , InferResult { constraints = []
                         , assumptions = As.singleton x tv } )

  PLit lt -> do
    let t = inferLit lt
    return (t, PType t $ PLit lt, mempty)

  PWild -> do
    tv <- fresh
    return ( tv
           , PType tv PWild
           , mempty )

  PAs n p -> do
    (t, p', InferResult cs as) <- inferPattern s p
    tv <- fresh
    return ( tv
          , PType tv $ PAs n p'
          , InferResult { constraints = cs ++ [EqConst tv t]
                        , assumptions = as `As.merge` As.singleton n tv } )

  PCon n ps ->
    case Env.lookupType n env of
      Just t -> do
        tv <- fresh
        (ts, ps', r) <- inferPatterns env ps
        return ( tv
               , PType tv $ PCon n ps'
               , r <> InferResult { constraints = [EqConst t (foldr TArr tv ts)]
                                  , assumptions = As.empty }
               )
      
      Nothing -> disclose $ One (_UndefinedConstructor # (n, l))

  PParen p -> do
    (t, p', r) <- inferPattern s p
    return ( t
           , PType t $ PParen p'
           , r)

  PLoc l' p -> do
    (t, p', r) <- inferPattern (env, l') p
    return ( t
           , PType t $ PLoc l' p'
           , r)

  PType t p -> do
    (t', _, r) <- inferPattern s p
    return (t
           , PType t p
           , r <> InferResult [EqConst t t'] As.empty)


-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

-- | Cannonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Type
closeOver = normalize . generalize Set.empty


extendMSet :: MonadReader (Set Text) m
           => Text -> m a -> m a
extendMSet x = local (Set.insert x)

extendsMSet :: MonadReader (Set Text) m
            => [Text] -> m a -> m a
extendsMSet xs = local (Set.union (Set.fromList xs))


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

genftv :: Int -> Type
genftv n =
  TKind KStar $ TVar $ pack (letters !! n)


fresh :: (MonadState s m, HasTypeState s)
      => m Type
fresh = do
  countfv += 1
  uses countfv genftv


-- This should instantiate a type by finding foralls,
-- generating a fresh type variable, subustituting the forall's
-- variable, and discarding the forall, leaving the type instantiated.
instantiate :: (MonadState s m, HasTypeState s)
            => Type -> m Type
instantiate =
  -- USES UNIPLATE, MAY NOT WORK
  transformM $ \x -> case x of
    TForall tvs t -> do
      tvs' <- mapM (const fresh) tvs
      let s = Subst $ Map.fromList (zip tvs tvs')
      return $ Subst.apply s t
    _ -> return x


-- Given a set of variables and some free variables from a type,
-- wrap that type in Foralls with those variables.
generalize :: Set Text -> Type -> Type
generalize free t = TForall as t
  where as = Set.toList $ ftv t `Set.difference` free


-- Normalizes names in a given type, from given names to generated
-- names wrapped in foralls.
normalize :: Type -> Type
normalize t1 =
  -- Wrap normalized type in foralls with new free variables
  TForall (Set.toList $ ftv t3) t3
  where
    -- First drop the foralls
    t2 = dropForall t1
    -- Build substitution map from free type variables in t2,
    -- to type variables with generated names.
    s = Subst.fromList $ zip (Set.toList $ ftv t2)
                             (map (TVar . pack) letters)
    -- Substitute names with generated names.
    t3 = Subst.apply s t2

-------------------------------------------------------------------------------
-- Constraint Solving
-------------------------------------------------------------------------------

solve :: ( MonadState s m, HasTypeState s
         , MonadChronicle (Bag e) m, AsTcErr e
         )
      => [Constraint] -> m Subst
solve [] = return Subst.empty
solve cs = solve' (nextSolvable cs)


nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs = fromJust (find solvable (chooseOne xs))
  where
    chooseOne xs = [(x, ys) | x <- xs, let ys = delete x xs]
    solvable (EqConst{}, _)      = True
    solvable (ExpInstConst{}, _) = True
    solvable (ImpInstConst ts ms t2, cs) = Set.null ((ftv t2 `Set.difference` ms) `Set.intersection` atv cs)


solve' :: ( MonadState s m, HasTypeState s
          , MonadChronicle (Bag e) m, AsTcErr e
          )
       => (Constraint, [Constraint]) -> m Subst
solve' = \case
  -- Equality Constraint
  (EqConst t1 t2, cs) -> do
    -- Unify t1 with t2.
    su1 <- unify t1 t2
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


unifyMany :: (MonadChronicle (Bag e) m, AsTcErr e)
          => [Type] -> [Type] -> m Subst
unifyMany [] [] = return Subst.empty
unifyMany (t1:ts1) (t2:ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyMany (Subst.apply su1 ts1) (Subst.apply su1 ts2)
     return $ su2 `Subst.compose` su1

unifyMany t1 t2 = disclose $ One (_UnificationMismatch # (t1, t2))


unify :: (MonadChronicle (Bag e) m, AsTcErr e)
        => Type -> Type -> m Subst
unify t1 t2
  | t1 == t2 = return Subst.empty
  | otherwise = case (t1, t2) of
      -- Throw away kinds (for now)
      -- Will modify when kinds are inferred during constraint inference
      (TKind _ t1, t2) ->  unify t1 t2
      (t1, TKind _ t2) -> unify t1 t2

      -- Throw away locations
      (t1, TLoc _ t2) -> unify t1 t2
      (TLoc _ t1, t2) -> unify t1 t2

      -- Throw away parens
      (TParen t1, t2) -> unify t1 t2
      (t1, TParen t2) -> unify t1 t2

      -- Throw away foralls
      (TForall _ t1, t2) ->  unify t1 t2
      (t1, TForall _ t2) -> unify t1 t2

      -- Unify variables
      (TVar v, t) -> v `bind` t
      (t, TVar v) -> v `bind` t

      -- Unify constructor
      (TCon v, t) -> v `bind` t
      (t, TCon v) -> v `bind` t

      -- Unify function types
      (TArr t1 t2, TArr t3 t4)    -> unifyMany [t1,t2] [t3,t4]
      (TLoli t1 t2, TLoli t3 t4)  -> unifyMany [t1,t2] [t3,t4]
      (TApp a1 b1, TApp a2 b2)    -> unifyMany [a1, b1] [a2, b2]

      -- No matches found, unfication has failed
      (t1, t2) -> confess $ One (_UnificationFail # (t1, t2))


bind :: (MonadChronicle (Bag e) m, AsTcErr e)
     => Text -> Type -> m Subst
bind a t | t == TVar a     = return Subst.empty
         | t == TCon a     = return Subst.empty
         | occursCheck a t = disclose $ One (_InfiniteType # (a, t))
         | otherwise       = return (Subst $ Map.singleton a t)
     

occursCheck :: FreeTypeVars a => Text -> a -> Bool
occursCheck a t = a `Set.member` ftv t


