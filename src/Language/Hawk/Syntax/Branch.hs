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
import Data.List.NonEmpty(NonEmpty)
import Language.Hawk.Syntax.Telescope
import Language.Hawk.Syntax.Literal

import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.PrettyPrint.Leijen.Text as PP


data Branches c a t v
  = ConBranches [ConBranch c a t v]
  | LitBranches (NonEmpty (LitBranch t v)) (t v)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


data ConBranch c a t v = ConBranch c (Telescope a t v) (Scope TeleVar t v)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data LitBranch t v = LitBranch Lit (t v)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


instance (PP.Pretty c, PP.Pretty a, PP.Pretty (t v), PP.Pretty v)
  => PP.Pretty (Branches c a t v) where

    pretty = \case
      ConBranches cs ->  PP.hsep (PP.pretty <$> cs)
      LitBranches ls def -> 
        PP.hsep $ (PP.pretty <$> (NonEmpty.toList ls))
                  ++ [PP.textStrict "_ ->" PP.<+> PP.pretty def]


instance (PP.Pretty c, PP.Pretty a, PP.Pretty (t v), PP.Pretty v)
  => PP.Pretty (ConBranch c a t v) where
    pretty (ConBranch c ts s) = undefined


instance (PP.Pretty (t v), PP.Pretty v)
  => PP.Pretty (LitBranch t v) where
    pretty (LitBranch l t) = undefined