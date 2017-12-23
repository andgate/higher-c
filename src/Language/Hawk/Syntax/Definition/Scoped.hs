{-# Language  DeriveFoldable
            , DeriveFunctor
            , DeriveTraversable
            , FlexibleContexts
            , FlexibleInstances
            , GADTs
            , OverloadedStrings
            , TemplateHaskell 
  #-}
module Language.Hawk.Syntax.Definition.Scoped where

import Bound
import Bound.Var
import Bound.Scope
import Control.Monad
import Data.Bifunctor
import Data.Bitraversable
import Data.Deriving
import Data.Functor.Classes
import Data.List.NonEmpty(NonEmpty)
import Data.Traversable
import Language.Hawk.Syntax.GlobalBind
import Language.Hawk.Syntax.Pattern
import Language.Hawk.Syntax.Pattern.Source

import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- | Pattern definition and clause

data PatDef clause
  = PatDef Abstract IsInstance (NonEmpty clause)
  deriving (Foldable, Functor, Show, Traversable)

data Clause b term v = Clause
  { clausePatterns ::  [Pat (Scope (Var PatVar b) term v) ()]
  , clauseScope :: Scope (Var PatVar b) term v
  } deriving (Show)


-- -----------------------------------------------------------------------------
-- | Abstract Annotation

data Abstract = Abstract | Concrete
  deriving (Eq, Ord, Show)

instance PP.Pretty Abstract where
  pretty Abstract = "abstract"
  pretty Concrete = "concrete"


-- -----------------------------------------------------------------------------
-- | Instance Annotation

data IsInstance
  = IsOrdinaryDefinition
  | IsInstance
  deriving (Eq, Ord, Show)

instance PP.Pretty IsInstance where
  pretty IsOrdinaryDefinition = mempty
  pretty IsInstance = "instance"

-- -----------------------------------------------------------------------------
-- | Instances

instance Traversable term => Functor (Clause b term) where fmap = fmapDefault
instance Traversable term => Foldable (Clause b term) where foldMap = foldMapDefault

instance Traversable term => Traversable (Clause b term) where
  traverse f (Clause pats s)
    = Clause <$> traverse (bitraverse (traverse f) pure) pats <*> traverse f s

instance (Eq1 term, Monad term, Eq b) => Eq1 (Clause b term) where
  liftEq f (Clause ps1 s1) (Clause ps2 s2) = liftEq (liftPatEq (liftEq f) (==)) ps1 ps2 && liftEq f s1 s2

instance GlobalBound (Clause b) where
  bound f g (Clause pats s) = Clause (first (bound f g) <$> pats) (bound f g s)

deriveEq1 ''PatDef