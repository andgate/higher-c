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
import Control.Monad.Morph
import Data.Deriving
import Data.Functor.Classes
import Data.Hashable
import Language.Hawk.Syntax.GlobalBind
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


-------------------------------------------------------------------------------
-- Instances

instance GlobalBound LetRec where
  bound f g (LetRec xs)
    = LetRec $ (\(LetBinding h s t) -> LetBinding h (bound f g s) (bind f g t)) <$> xs

instance MFunctor LetRec where
  hoist f (LetRec xs)
    = LetRec $ (\(LetBinding h s t) -> LetBinding h (hoist f s) (f t)) <$> xs


$(return mempty)

instance (Eq1 expr, Monad expr) => Eq1 (LetRec expr) where
  liftEq = $(makeLiftEq ''LetRec)

instance (Ord1 expr, Monad expr) => Ord1 (LetRec expr) where
  liftCompare = $(makeLiftCompare ''LetRec)

instance (Show1 expr, Monad expr) => Show1 (LetRec expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''LetRec)

instance (Eq1 expr, Monad expr) => Eq1 (LetBinding expr) where
  liftEq = $(makeLiftEq ''LetBinding)

instance (Ord1 expr, Monad expr) => Ord1 (LetBinding expr) where
  liftCompare = $(makeLiftCompare ''LetBinding)

instance (Show1 expr, Monad expr) => Show1 (LetBinding expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''LetBinding)


-------------------------------------------------------------------------------
-- Pretty Printing

instance (PP.Pretty (t v), PP.Pretty v) => PP.Pretty (LetRec t v) where
  pretty (LetRec cs) = PP.cat $ map PP.pretty cs

instance (PP.Pretty (t v), PP.Pretty v) => PP.Pretty (LetBinding t v) where
  pretty (LetBinding _ s t) = undefined