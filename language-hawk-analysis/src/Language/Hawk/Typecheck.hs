{-# Language GeneralizedNewtypeDeriving #-}
module Language.Hawk.Typecheck where

import Control.Monad.State
import Control.Monad.Except
import Language.Hawk.Typecheck.Error
import Language.Hawk.Syntax.Suspension


newtype Tc a = Tc { runTc :: StateT () (Except TcError) a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState ()
           , MonadError TcError
           )

typecheckTerm :: (Term a) -> Tc (Type a)
typecheckTerm m = undefined

inferTerm :: Term a -> Tc (Type a)
inferTerm m = undefined