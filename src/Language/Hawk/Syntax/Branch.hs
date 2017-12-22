{-# LANGUAGE  DeriveFoldable
            , DeriveFunctor
            , DeriveTraversable
            , FlexibleContexts
            , MonadComprehensions
            , Rank2Types
            , OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.Syntax.Branch where

import Bound
import Data.List.NonEmpty(NonEmpty)
import Language.Hawk.Syntax.Telescope
import Language.Hawk.Syntax.Literal

import qualified Data.List.NonEmpty as NonEmpty


data Branches c a t v
  = ConBranches [ConBranch c a t v]
  | LitBranches (NonEmpty (LitBranch t v)) (t v)


data ConBranch c a t v = ConBranch c (Telescope a t ) (Scope TeleVar t v)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data LitBranch t v = LitBranch Lit (t v)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)