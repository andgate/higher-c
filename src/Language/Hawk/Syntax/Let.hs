{-# LANGUAGE  DeriveFoldable
            , DeriveFunctor
            , DeriveTraversable
            , GeneralizedNewtypeDeriving
            , OverloadedStrings
            , RankNTypes
            , TemplateHaskell
  #-}
module Language.Hawk.Syntax.Let where

import Bound
import Bound.Scope
import Data.Hashable
import Language.Hawk.Syntax.Name

import qualified Text.PrettyPrint.Leijen.Text as PP

-------------------------------------------------------------------------------
-- Let Types

type LetScope = Scope LetVar
newtype LetVar = LetVar Int
  deriving (Eq, Enum, Hashable, Ord, Show, Num)

unLetVar :: LetVar -> Int
unLetVar (LetVar i) = i

newtype LetRec t v = LetRec [LetBinding t v]
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

unLetRec :: LetRec t v -> [LetBinding t v]
unLetRec (LetRec xs) = xs

data LetBinding t v = LetBinding !NameHint !(Scope LetVar t v) (t v)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)


instance (PP.Pretty (t v), PP.Pretty v) => PP.Pretty (LetRec t v) where
  pretty (LetRec cs) = PP.cat $ map PP.pretty cs

instance (PP.Pretty (t v), PP.Pretty v) => PP.Pretty (LetBinding t v) where
  pretty (LetBinding _ s t) = undefined