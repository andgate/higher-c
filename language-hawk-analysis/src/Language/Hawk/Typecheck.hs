{-# Language GeneralizedNewtypeDeriving #-}
module Language.Hawk.Typecheck where

import Control.Monad.State
import Control.Monad.Except
import Language.Hawk.Typecheck.Error
import Language.Hawk.Syntax.Bound


newtype Tc a = Tc { runTc :: StateT () (Except TcError) a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState ()
           , MonadError TcError
           )

typecheckTerm :: Term -> Tc Type
typecheckTerm m = undefined

inferTerm :: Term -> Tc Type
inferTerm m = undefined