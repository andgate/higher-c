{-# LANGUAGE  DeriveFunctor
            , DeriveFoldable
            , DeriveTraversable
            , MonadComprehensions
            , OverloadedStrings
            , GeneralizedNewtypeDeriving
  #-}
module Language.Hawk.Syntax.Pattern.Scoped where

import Bound
import Data.Hashable
import Data.Text
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Name


type PatScope = Scope PatVar
newtype PatVar = PatVar Int
  deriving (Eq, Enum, Hashable, Ord, Show, Num)

unPatVar :: PatVar -> Int
unPatVar (PatVar i) = i

------------------------------------------------------------------------------
-- Scoped Pattern
data Pat t b
  = PVar NameHint b
  | PLit Lit
  | PCon Text [t] [(Pat t b, t)]
  | PView t (Pat t b)
  deriving(Foldable, Functor, Traversable)
