{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.Hawk.Data.Emittable where

class Emittable a b | a -> b where
  emit :: a -> b