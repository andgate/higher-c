{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeFamilies, TypeOperators, RankNTypes #-}
module Data.List.Sized where

import Data.Singletons
import Data.Nat
import Unsafe.Coerce

 

data SList (n :: Nat) a where
  Nil   :: SList Z a
  (:#) :: a -> SList n a -> SList (S n) a



mono :: a -> SList (S Z) a
mono = (:# Nil)


fromList :: [a] -> SList n a
fromList [] = unsafeCoerce $ Nil
fromList (x:xs) = unsafeCoerce (x :# fromList xs)


toList :: SList n a -> [a]
toList Nil = []
toList (x :# xs) = x : toList xs
