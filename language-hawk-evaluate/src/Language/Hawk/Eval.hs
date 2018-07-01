{-# LANGUAGE LambdaCase
           , GeneralizedNewtypeDeriving
 #-}
module Language.Hawk.Eval where


import Data.Either (fromRight)
import Data.Text (Text)
import Language.Hawk.Syntax.Suspension
import Language.Hawk.Syntax.Prim


data Eval a
  = EvalLam Text (Term (Var a))
  | EvalNeutral a [Term a]
  | EvalInstrElim PrimInstr [Term a]
  | EvalVal PrimVal

evalTerm :: Term a -> Eval a
evalTerm = undefined
{-
eval = \case
  TVar v -> EvalNeutral v []
  TVal v -> EvalValue v
  TLam v body -> EvalLam v body
  TApp f a -> case eval (removeSusp f) of
    EvalNeutral v as -> EvalNeutral v (as ++ [a])
    EvalInstr i t -> Eval 
  TPrim PrimInstr t1 t2
  TAnn (Term a) Type
  TLoc Loc (Term a)
  -}

evalSyntax :: Term a -> Eval a
evalSyntax = undefined