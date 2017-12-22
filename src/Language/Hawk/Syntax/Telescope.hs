{-# LANGUAGE  DeriveFoldable
            , DeriveFunctor
            , DeriveTraversable
            , FlexibleContexts
            , GADTs
            , GeneralizedNewtypeDeriving
            , OverloadedStrings
            , Rank2Types
            , TemplateHaskell
            , ViewPatterns
  #-}
module Language.Hawk.Syntax.Telescope where

import Bound
import Bound.Scope
import Bound.Var
import Data.Hashable
import Language.Hawk.Syntax.Name


-------------------------------------------------------------------------------------
-- Telescope

newtype TeleVar = TeleVar Int
  deriving (Eq, Enum, Hashable, Ord, Show, Num)

unTeleVar :: TeleVar -> Int
unTeleVar (TeleVar i) = i


newtype Telescope a t v = Telescope [TeleArg a t v]
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

data TeleArg a t v = TeleArg !NameHint !a !(Scope TeleVar t v)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

unTelescope :: Telescope a t v -> [TeleArg a t v]
unTelescope (Telescope xs) = xs
