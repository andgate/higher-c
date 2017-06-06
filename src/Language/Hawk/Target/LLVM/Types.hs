{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
  #-}
module Language.Hawk.Target.LLVM.Types where


class Emittable a b | a -> b where
    emit :: a -> b