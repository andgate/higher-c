{-# Language  GeneralizedNewtypeDeriving #-}
module Language.Hawk.Syntax.Pattern where

import Bound
import Control.Monad.State
import Data.Bitraversable
import Data.Functor.Identity
import Data.Hashable
import Data.List (elemIndex)

------------------------------------------------------------------------------
-- Pattern Scoping

type PatScope = Scope PatVar
newtype PatVar = PatVar Int
  deriving (Eq, Enum, Hashable, Ord, Show, Num)

unPatVar :: PatVar -> Int
unPatVar (PatVar i) = i

------------------------------------------------------------------------------
-- Scope Helpers

patternAbstraction
  :: (Eq b, Hashable b)
  => [b] -> b -> Maybe PatVar
patternAbstraction vs v = PatVar <$> elemIndex v vs


abstractPatternsTypes
  :: (Bitraversable pat, Eq v, Hashable v, Monad typ, Traversable t)
  => [v]
  -> t (p, pat (typ v) b)
  -> t (p, pat (PatScope typ v) b)
abstractPatternsTypes vars
  = flip evalState 0 . traverse (bitraverse pure (bitraverse (abstractType vars) inc))
  where
    abstractType
      :: (Eq v, Hashable v, Monad typ)
      => [v] -> typ v -> State Int (Scope PatVar typ v)
    abstractType vs typ = do
      prefix <- get
      let abstr v = case elemIndex v vs of
            Just i | i < prefix -> Just $ PatVar i
            _ -> Nothing
      return $ abstract abstr typ

    inc b = do
      n <- get
      put $! n + 1
      pure b


abstractPatternTypes
  :: (Bitraversable pat, Eq v, Hashable v, Monad typ)
  => [v]
  -> pat (typ v) b
  -> pat (PatScope typ v) b
abstractPatternTypes vars
  = snd
  . runIdentity
  . abstractPatternsTypes vars
  . Identity
  . (,) ()