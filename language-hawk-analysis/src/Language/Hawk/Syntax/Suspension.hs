{-# Language GADTs
           , OverloadedStrings
           , LambdaCase
           , ExistentialQuantification
           , ScopedTypeVariables
  #-}
module Language.Hawk.Syntax.Suspension where

import Data.Text (Text)
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Prim

data Var a
  = B
  | F a

type Type = Term

data Term a where
  Syntax :: Syntax a -> Term a
  Susp :: Env from to -> Syntax from -> Term to

data Syntax a
  = Type
  | Linear
  | TVar a
  | TCon Text
  | TVal PrimVal
  | TPrim PrimInstr (Term a) (Term a)
  
  | TApp   (Term a) (Term a)
  | TLam   Text (Maybe (Type Text)) (Term (Var a))
  | TPi    Text (Type a) (Type (Var a))
  | TSigma (Type a) (Type a)

  | TAnn (Term a) (Type Text)
  | TParen (Term a)
  | TLoc Loc (Term a)


-- Smart Constructors
var :: a -> Term a
var v = Syntax (TVar v)

lam :: Text -> Term (Var a) -> Term a
lam n body = Syntax (TLam n Nothing body)

lamty :: Text -> Type Text -> Term (Var a) -> Term a
lamty n ty body = Syntax (TLam n (Just ty) body)


------------------------------------------------------------------------------------
-- Environment

data Weaken from to where
  WeakenZero :: Weaken a a
  WeakenSucc :: Weaken from to -> Weaken from (Var to)


data CanonicalEnv from to where
  EnvNil :: Weaken from to -> CanonicalEnv from to
  EnvCons :: Text -> Term to -> Env from to -> CanonicalEnv (Var from) to


data Env from to where
  EnvCanonical :: CanonicalEnv from to -> Env from to
  EnvComp :: Env a b -> Env b c -> Env a c



-- Environment Helpers

susp :: Env from to -> Term from -> Term to
susp env = \case
  Syntax t -> Susp env t
  Susp env' t -> Susp (EnvComp env' env) t

envNil :: Env a a
envNil = EnvCanonical (EnvNil WeakenZero)

envCons :: Text -> Term to -> Env from to -> Env (Var from) to
envCons v t env = EnvCanonical (EnvCons v t env)

envComp :: Env a b -> Env b c -> Env a c
envComp = EnvComp

envWeaken :: Env a b -> Weaken b c -> Env a c
envWeaken env wk = envComp env (EnvCanonical (EnvNil wk))

envAbs :: Text -> Env from to -> Env (Var from) (Var to)
envAbs v env = envCons v (var B) (envWeaken env (WeakenSucc WeakenZero))


evalEnv :: Env from to -> CanonicalEnv from to
evalEnv = \case
  EnvCanonical env -> env
  EnvComp env1 env2 -> goComp (evalEnv env1) env2
  where
    goComp :: CanonicalEnv a b -> Env b c -> CanonicalEnv a c
    goComp (EnvNil wk) env2 = goCompWeaken wk env2
    goComp (EnvCons n e env1) env2 = EnvCons n (susp env2 e) (EnvComp env1 env2)

    -- Composes a weakening and an environment.
    goCompWeaken :: Weaken a b -> Env b c -> CanonicalEnv a c
    goCompWeaken wk = \case
      EnvCanonical (EnvNil wk') -> EnvNil (compWeaken wk wk')
      EnvCanonical (EnvCons v e env) -> case wk of
        WeakenZero -> EnvCons v e env
        WeakenSucc wk' -> goCompWeaken wk' env
      EnvComp env1 env2 -> goComp (goCompWeaken wk env1) env2

    compWeaken :: Weaken a b -> Weaken b c -> Weaken a c
    compWeaken wk1 WeakenZero = wk1
    compWeaken wk1 (WeakenSucc wk2) = WeakenSucc (compWeaken wk1 wk2)


envLookup :: Env from to -> from -> Term to
envLookup env0 v = case evalEnv env0 of
  EnvNil wk -> var (weakenVar wk v)
  EnvCons _ t env -> case v of
    B -> t
    F v' -> envLookup env v'

weakenVar :: Weaken from to -> from -> to
weakenVar wk0 v = case wk0 of
  WeakenZero -> v
  WeakenSucc wk -> F (weakenVar wk v)


removeSusp :: Term a -> Syntax a
removeSusp = \case
  Syntax t -> t
  Susp env t0 -> case t0 of
    Type    -> Type
    Linear  -> Linear
    TVar v  -> removeSusp (envLookup env v)
    TCon c  -> TCon c
    TVal v  -> TVal v
    
    TPrim i t t'    -> TPrim i (susp env t) (susp env t')
    TApp t t'       -> TApp (susp env t) (susp env t')
    TLam v mty body -> TLam v (susp envNil <$> mty) (susp (envAbs v env) body)
    TPi n ty ty'    -> TPi n (susp env ty) (susp (envAbs n env) ty')
    TSigma ty ty'   -> TSigma (susp env ty) (susp env ty')

    TAnn tm ty  -> TAnn (susp env tm) (susp envNil ty)
    TParen t    -> TParen (susp env t)
    TLoc l t    -> TLoc l (susp env t) 


removeAllSusps :: Term a -> Term a
removeAllSusps e = Syntax $ case removeSusp e of
  Type    -> Type
  Linear  -> Linear
  TVar v  -> TVar v
  TCon c  -> TCon c
  TVal v  -> TVal v
  
  TPrim i t t'    -> TPrim i (removeAllSusps t) (removeAllSusps t')
  TApp t t'       -> TApp (removeAllSusps t) (removeAllSusps t')
  TLam v mty body -> TLam v (removeAllSusps <$> mty) (removeAllSusps body)
  TPi n ty ty'    -> TPi n (removeAllSusps ty) (removeAllSusps ty')
  TSigma ty ty'   -> TSigma (removeAllSusps ty) (removeAllSusps ty')

  TAnn tm ty  -> TAnn (removeAllSusps tm) (removeAllSusps ty)
  TParen t    -> TParen (removeAllSusps t)
  TLoc l t    -> TLoc l (removeAllSusps t)


subst :: Text -> Term a -> Term (Var a) -> Term a
subst v tm body = susp (envCons v tm envNil) body