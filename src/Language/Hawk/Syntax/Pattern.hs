{-# Language  GeneralizedNewtypeDeriving #-}
module Language.Hawk.Syntax.Pattern where

import Bound
import Data.Hashable

------------------------------------------------------------------------------
-- Pattern Scope and Scope Variable

type PatScope = Scope PatVar
newtype PatVar = PatVar Int
  deriving (Eq, Enum, Hashable, Ord, Show, Num)

unPatVar :: PatVar -> Int
unPatVar (PatVar i) = i