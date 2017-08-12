{-# Language  LambdaCase #-}
module Language.Hawk.TypeCheck.Equality where

import Control.Monad.Reader
import Language.Hawk.Syntax.Term
import Language.Hawk.TypeCheck.Environment

import Unbound.Generics.LocallyNameless


whnf :: (MonadReader r m, HasTcEnv r)
     => Term -> m Term
whnf = whnf' False


whnf' :: (MonadReader r m, HasTcEnv r)
      => Bool -> Term -> m Term
whnf' b = \case
  Var x -> do
    maybeDef <- lookupDef x
    case maybeDef of
      Just d -> whnf' b d
      _ ->
        if b then do
            maybeRecDef <- lookupRecDef x
            case maybeRecDef of
              Just d -> whnf' False d
              _ -> return (Var x)
          else
            return (Var x)

  _ -> undefined
