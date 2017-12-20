{-# LANGUAGE  FlexibleContexts
            , LambdaCase
            , OverloadedStrings
  #-}
module Language.CPS.LambdaLift where

import Control.Lens
import Control.Monad.Gen
import Data.Monoid
import Data.Text (Text)
import Language.CPS.Syntax
import Language.Hawk.NameGen


-- Syntax tree based on lambda calculus


lambdaLiftValue :: (MonadGen Int m)
    => Text -> Value -> m ([Lambda], Value)
lambdaLiftValue def_n = \case
  Use n -> return ([], Use n)
  Dup n -> return ([], Dup n)
  Lit l -> return ([], Lit l)
  Var n -> return ([], Var n)
  
  Lam x e -> do
    (ds, e') <- lambdaLiftTerm def_n e
    n <- newName
    let n' = def_n <> "#lam." <> n
        d = Def n' $ Halt $ Lam x e'
    return (d:ds, Var n')


lambdaLiftTerm :: (MonadGen Int m)
    => Text -> Term -> m ([Lambda], Term)
lambdaLiftTerm def_n = \case
  Let n b e -> do
    (ds1, b') <- lambdaLiftBinder def_n b
    (ds2, e') <- lambdaLiftTerm def_n e
    return (ds1++ds2, Let n b' e')
    
  If p a b -> do
    (ds1, p') <- lambdaLiftValue def_n p
    (ds2, b') <- lambdaLiftTerm def_n a
    (ds3, a') <- lambdaLiftTerm def_n b
    return (ds1++ds2++ds3, If p' a' b')    
  
  Jump a b -> do
    (ds1, a') <- lambdaLiftValue def_n a
    (ds2, b') <- lambdaLiftValue def_n b
    return (ds1++ds2, Jump a' b')
    
  Halt e -> do
    (ds, e') <- lambdaLiftValue def_n e
    return (ds, Halt e')



lambdaLiftBinder :: (MonadGen Int m)
    => Text -> Binder -> m ([Lambda], Binder)
lambdaLiftBinder def_n = \case
  PrimBind i a b -> do
    (ds1, a') <- lambdaLiftValue def_n a
    (ds2, b') <- lambdaLiftValue def_n b
    return (ds1++ds2, PrimBind i a' b')
    
  ProjL n -> return ([], ProjL n)
  ProjR n -> return ([], ProjR n)

  Pair a b -> do
    (ds1, a') <- lambdaLiftValue def_n a
    (ds2, b') <- lambdaLiftValue def_n b
    return (ds1++ds2, Pair a' b')
