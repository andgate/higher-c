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

typecheckMany :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
                    , MonadChronicle (Bag e) m, AsTcErr e )
                 => Image -> m Image
typecheckMany img = evalStateT m env
  where
    sigsx = img^.imgSigs
    sigsx' = sigsx & each.sigType %~ closeOver
    
    env = Env.fromSigs sigsx'

    m = do
      img' <- mapMOf (imgFns.each) f img
      logInfo (_TcComplete # ())
      return img'
      
    f (Fn n xs e) = do
      env <- get
      (e', env') <- typecheck env n e
      put env'
      return $ Fn n xs e'


typecheck :: ( MonadLog (WithSeverity msg) m, AsTcMsg msg
             , MonadChronicle (Bag e) m, AsTcErr e
             )
          => Env -> Text -> Exp -> m (Exp, Env)
typecheck env n e = run $ do
  (t, e', InferResult cs as) <- inferConstraints e
  let unbounds = Set.fromList (As.keys as) `Set.difference` Set.fromList (Env.keys env)
  unless (Set.null unbounds)
         $ disclose $ One (_UnboundVariable # Set.findMin unbounds)
  let cs' = [ ExpInstConst t s | (x, s) <- Env.toList env
                               , t <- As.lookup x as ]

  s <- solve (cs ++ cs')
  let e'' = Subst.apply s e'
      t'  = closeOver $ Subst.apply s t
      env' = Env.extend env (n, t')
      
  return (e'', env')

  where
    run m = evalStateT (runReaderT m Set.empty) (def :: TypeState)


-----------------------------------------------------------------------
-- Constraint Inference
-----------------------------------------------------------------------

data InferResult
  = InferResult
      { constraints :: [Constraint]
      , assumptions :: Assumption
      } deriving(Show)


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


inferConstraints
  :: ( MonadReader (Set Text) m
     , MonadState s m, HasTypeState s
     , MonadLog (WithSeverity msg) m, AsTcMsg msg
     , MonadChronicle (Bag e) m, AsTcErr e )
  => Exp -> m (Type, Exp, InferResult)
inferConstraints = \case
  EVar x -> do
    tv <- fresh
    return ( tv
           , EType tv $ EVar x
           , InferResult { constraints = []
                         , assumptions = As.singleton x tv })


  EApp e1 e2 -> do
    tv <- fresh
    (t1, e1', r1) <- inferConstraints e1
    (t2, e2', r2) <- inferConstraints e2
    return ( tv
           , EType tv $ EApp e1' e2'
           , r1 <> r2 <> InferResult { constraints = [EqConst t1 (t2 `TArr` tv)]
                                    , assumptions = As.empty }
           )


  ELam x e -> do
    tv@(TVar a) <- fresh
    (t, e', InferResult cs as) <- extendMSet a $ inferConstraints e
    let as' = as `As.remove` x 
    return ( tv `TArr` t
           , EType (tv `TArr` t) $ ELam x e'
           , InferResult { constraints = cs ++ [EqConst t' tv | t' <- As.lookup x as]
                        , assumptions = as' }
           )
    

  ELet (x, e1) e2 -> do
    (t1, e1', InferResult cs1 as1) <- inferConstraints e1
    (t2, e2', InferResult cs2 as2) <- inferConstraints e2
    ms <- ask
    return ( t2
           , EType t2 $ ELet (x, e1') e2'
           , InferResult { constraints = cs1 ++ cs2 ++ [ImpInstConst t' ms t1 | t' <- As.lookup x as2]
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
           , InferResult
             { constraints = []
             , assumptions = As.singleton n tv })

  EPrim i ->
    let t = inferPrimInstr i
    in return ( t
              , EType t $ EPrim i
              , mempty)

  EIf e1 e2 e3 -> do
    (t1, e1', r1) <- inferConstraints e1
    (t2, e2', r2) <- inferConstraints e2
    (t3, e3', r3) <- inferConstraints e3
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
                         , assumptions = As.singleton n tv } )

  EFree n e -> do
    (t, e', r) <- inferConstraints e
    return ( t
           , EType t $ EFree n e'
           , r)

  EType t e -> do
    (t', _, r) <- inferConstraints e
    return ( t
           , EType t e
           , r <> InferResult [EqConst t t'] As.empty)

  ETLit _ _ -> error "Type inferenece doesn't work on type literals"

  ELoc l e -> do
    (t, e', r) <- inferConstraints e
    return (t, ELoc l e', r)

  EParen e -> do
    (t, e', r) <- inferConstraints e
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



-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

-- | Cannonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Type
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
            => Type -> m Type
instantiate =
  transformM $ \x -> case x of
    TForall tv t -> do
      tv' <- fresh
      let s = Subst $ Map.singleton tv tv'
      return $ Subst.apply s t
    _ -> return x


-- Given a set of variables and some free variables from a type,
-- wrap that type in Foralls with those variables.
generalize :: Set Text -> Type -> Type
generalize free t = foldr TForall t as
  where as = Set.toList $ ftv t `Set.difference` free


-- Normalizes names in a given type, from given names to generated
-- names wrapped in foralls.
normalize :: Type -> Type
normalize t =
  -- Wrap foralls with the generated type variables
  -- over the normalized typed.
  foldr (TForall . snd) t'' ord
  where
    -- First drop the foralls
    t' = dropForall t
    -- Create a list of free names, paired with generated names
    ord = zip (nub . Set.toList $ ftv t') (map pack letters)
    -- Build substitution map from previous vars to generated vars
    s = Subst.fromList $ map (second TVar) ord
    -- Apply substition over our type
    t'' = Subst.apply s t'

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
      -- Unify variables
      (TVar v, t) -> v `bind` t
      (t, TVar v) -> v `bind` t

      -- Unify arrows
      (TArr t1 t2, TArr t3 t4) -> unifyMany [t1,t2] [t3,t4]
      (TLoli t1 t2, TLoli t3 t4) -> unifyMany [t1,t2] [t3,t4]

      -- Unify kinded types
      -- This should probably test kinds too
      ts@(TKind k1 t1, t2) -> unify t1 t2
--        unless (k1 `ksub` kind t2)
--               (confess $ One (_UnificationFail # ts))
--        unifies t1 t2
      (t1, TKind _ t2) -> unify t1 t2

      -- Unify located types
      (t1, TLoc _ t2) -> unify t1 t2
      (TLoc _ t1, t2) -> unify t1 t2

      -- Unify types in parens
      (TParen t1, t2) -> unify t1 t2
      (t1, TParen t2) -> unify t1 t2

      (t1, t2) -> confess $ One (_UnificationFail # (t1, t2))


bind :: (MonadChronicle (Bag e) m, AsTcErr e)
     => Text -> Type -> m Subst
bind a t | t == TVar a     = return Subst.empty
         | occursCheck a t = disclose $ One (_InfiniteType # (a, t))
         | otherwise       = return (Subst $ Map.singleton a t)
     

occursCheck :: FreeTypeVars a => Text -> a -> Bool
occursCheck a t = a `Set.member` ftv t


