{-# LANGUAGE LambdaCase
           , GeneralizedNewtypeDeriving
 #-}
module Language.Hawk.Eval where


import Data.List (foldl')
import Data.Either (fromRight)
import Data.Text (Text)
import Language.Hawk.Syntax.Suspension
import Language.Hawk.Syntax.Prim


data Eval a
  = EvalLam Text (Term (Var a))
  | EvalPi Text (Type (Var a))
  | EvalNeutral a [Term a]
  | EvalCon Text [Term a]
  | EvalVal PrimVal
  | EvalType
  | EvalLinear

eval :: Syntax a -> Eval a
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

  TLam v mt body ->
    EvalLam v body

  TPi v _ body ->
    EvalPi v body
  
  TSigma t1 t2 ->
    EvalType

  TAnn tm ty -> eval (removeSusp tm) -- ignore annotations!
  TParen t -> eval (removeSusp t)
  TLoc l t -> eval (removeSusp t)


evalToSyntax :: Eval a -> Syntax a
evalToSyntax = \case
  EvalLam n body -> TLam n Nothing body
  EvalNeutral v args -> foldl' (\e -> TApp (Syntax e)) (TVar v) args
