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
import Control.Monad.Morph
import Data.Deriving
import Data.Functor.Classes
import Data.Hashable
import Language.Hawk.Syntax.GlobalBind
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

-- -----------------------------------------------------------------------------
-- | Instances

instance GlobalBound (Telescope a) where
  bound f g (Telescope tele)
    = Telescope $ (\(TeleArg h a s) -> TeleArg h a $ bound f g s) <$> tele

instance MFunctor (Telescope a) where
  hoist f (Telescope xs)
    = Telescope $ (\(TeleArg h p s) -> TeleArg h p $ hoist f s) <$> xs

$(return mempty)

instance (Eq anno, Eq1 expr, Monad expr) => Eq1 (Telescope anno expr) where
  liftEq = $(makeLiftEq ''Telescope)

instance (Ord anno, Ord1 expr, Monad expr) => Ord1 (Telescope anno expr) where
  liftCompare = $(makeLiftCompare ''Telescope)

instance (Show anno, Show1 expr, Monad expr) => Show1 (Telescope anno expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''Telescope)

instance (Eq anno, Eq1 expr, Monad expr) => Eq1 (TeleArg anno expr) where
  liftEq = $(makeLiftEq ''TeleArg)

instance (Ord anno, Ord1 expr, Monad expr) => Ord1 (TeleArg anno expr) where
  liftCompare = $(makeLiftCompare ''TeleArg)

instance (Show anno, Show1 expr, Monad expr) => Show1 (TeleArg anno expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''TeleArg)


-- -----------------------------------------------------------------------------
-- | Pretty Instances