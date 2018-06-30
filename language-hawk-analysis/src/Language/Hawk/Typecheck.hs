{-# Language GeneralizedNewtypeDeriving #-}
module Language.Hawk.Typecheck where

import Control.Monad.State
import Control.Monad.Except
import Language.Hawk.Typecheck.Error
import Language.Hawk.Syntax.Abstract


data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

type Context = [(Name, Info)]

newtype Tc a = Tc { runTc :: StateT Context (Except TcError) a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState Context
           , MonadError TcError
           )

typecheckTerm :: TermChk -> Tc Type
typecheckTerm m = undefined

inferTerm :: TermInf -> Tc Type
inferTerm m = undefined

inferKind :: Kind -> Tc ()
inferKind k = return ()