{-# Language GeneralizedNewtypeDeriving #-}
module Language.Hawk.Typecheck where

import Control.Monad.State
import Control.Monad.Except
import Language.Hawk.Syntax.Abstract


data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

type Context = [(Name, Info)]

newtype Tc a = Tc { runTc :: StateT Context (Except String) a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState Context
           , MonadError String
           )

typecheck :: Module -> Tc Module
typecheck m = undefined

infer :: Module -> Tc Module
infer m = undefined