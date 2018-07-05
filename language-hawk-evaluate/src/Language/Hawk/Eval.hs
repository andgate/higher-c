{-# LANGUAGE LambdaCase
           , GeneralizedNewtypeDeriving
 #-}
module Language.Hawk.Eval where


import Data.List (foldl')
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Map.Strict (Map)

import Language.Hawk.Closure
import Language.Hawk.Syntax.Bound
import Language.Hawk.Syntax.Prim
import Language.Hawk.Value


eval :: Closure -> Term -> Value
eval clos = \case
  _ -> undefined
{-
eval = \case
  Type   -> EvalType
  Linear -> EvalLinear
  
  TVar v -> EvalNeutral v []
  TCon n -> EvalCon n []
  TVal v -> EvalVal v
  
  TPrim i t1 t2 ->
    let (EvalVal v1) = eval (removeSusp t1)
        (EvalVal v2) = eval (removeSusp t2)
    in EvalVal $ evalInstr (i, v1, v2)

  TApp f a -> case eval (removeSusp f) of
    EvalNeutral v as -> EvalNeutral v (as ++ [a])
    EvalCon c as -> EvalCon c (as ++ [a])
    EvalLam v body -> eval . removeSusp $ subst v a body
    EvalPi v body  -> eval . removeSusp $ subst v a body
    EvalVal v -> EvalVal v
    EvalType -> EvalType
    EvalLinear -> EvalLinear

  TLam v _ body ->
    EvalLam v body

  TPi (v, Explicit) _ body ->
    EvalPi v body
  
  TSigma v t1 t2 ->
    EvalType

  TLet v t body -> eval $ TApp (lam v body) t
  TArrow t1 t2 -> undefined
  TTuple t1 t2  -> undefined

  TAnn tm ty -> eval (removeSusp tm) -- ignore annotations!
  TLoc l t -> eval (removeSusp t)


evalToSyntax :: Eval a -> Syntax a
evalToSyntax = \case
  EvalLam n body -> TLam n Nothing body
  EvalNeutral v args -> foldl' (\e -> TApp (Syntax e)) (TVar v) args


-}