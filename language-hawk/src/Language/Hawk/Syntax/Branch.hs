{-# LANGUAGE  DeriveFoldable
            , DeriveFunctor
            , DeriveTraversable
            , FlexibleContexts
            , MonadComprehensions
            , Rank2Types
            , OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.Syntax.Branch where

import Bound
import Bound.Scope
import Control.Monad.Morph
import Data.Deriving
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty)
import Data.Text.Prettyprint.Doc
import Language.Hawk.Syntax.GlobalBind
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Telescope

import qualified Data.List.NonEmpty as NonEmpty


data Branches c a t v
  = ConBranches [ConBranch c a t v]
  | LitBranches (NonEmpty (LitBranch t v)) (t v)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


data ConBranch c a t v = ConBranch c (Telescope a t v) (Scope TeleVar t v)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data LitBranch t v = LitBranch Lit (t v)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


-------------------------------------------------------------------------------
-- Instances

instance MFunctor (Branches c p) where
  hoist f (ConBranches cbrs)
    = ConBranches [ConBranch c (hoist f tele) (hoist f s) | ConBranch c tele s <- cbrs]
  hoist f (LitBranches lbrs def)
    = LitBranches [LitBranch l (f e) | LitBranch l e <- lbrs] $ f def

instance GlobalBound (Branches c a) where
  bound f g (ConBranches cbrs) = ConBranches $ bound f g <$> cbrs
  bound f g (LitBranches lbrs def) = LitBranches
    (bound f g <$> lbrs)
    (bind f g def)

instance GlobalBound (ConBranch c a) where
  bound f g (ConBranch c a s) = ConBranch c (bound f g a) (bound f g s)
instance GlobalBound LitBranch where
  bound f g (LitBranch l s) = LitBranch l (bind f g s)

$(return mempty)

instance (Eq c, Eq a, Eq1 expr, Monad expr) => Eq1 (Branches c a expr) where
  liftEq = $(makeLiftEq ''Branches)
instance (Ord c, Ord a, Ord1 expr, Monad expr) => Ord1 (Branches c a expr) where
  liftCompare = $(makeLiftCompare ''Branches)
instance (Show c, Show a, Show1 expr, Monad expr) => Show1 (Branches c a expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''Branches)

instance (Eq c, Eq a, Eq1 expr, Monad expr) => Eq1 (ConBranch c a expr) where
  liftEq = $(makeLiftEq ''ConBranch)
instance (Ord c, Ord a, Ord1 expr, Monad expr) => Ord1 (ConBranch c a expr) where
  liftCompare = $(makeLiftCompare ''ConBranch)
instance (Show c, Show a, Show1 expr, Monad expr) => Show1 (ConBranch c a expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''ConBranch)

instance Eq1 expr => Eq1 (LitBranch expr) where
  liftEq = $(makeLiftEq ''LitBranch)
instance Ord1 expr => Ord1 (LitBranch expr) where
  liftCompare = $(makeLiftCompare ''LitBranch)
instance Show1 expr => Show1 (LitBranch expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''LitBranch)


-------------------------------------------------------------------------------
-- Pretty Printing

instance (Pretty c, Pretty a, Pretty (t v), Pretty v)
  => Pretty (Branches c a t v) where

  pretty = \case
    ConBranches cs ->  hsep (pretty <$> cs)
    LitBranches ls def -> 
      hsep $ (pretty <$> NonEmpty.toList ls)
                ++ ["_ ->" <+> pretty def]


instance (Pretty c, Pretty a, Pretty (t v), Pretty v)
  => Pretty (ConBranch c a t v) where
    pretty (ConBranch c ts s) = undefined


instance (Pretty (t v), Pretty v)
  => Pretty (LitBranch t v) where
    pretty (LitBranch l t) = undefined