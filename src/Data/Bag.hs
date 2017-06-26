module Data.Bag where

import Prelude hiding (map)

import Data.Semigroup

data Bag a 
  = Empty
  | One a
  | Two (Bag a) (Bag a)
  | Many [a]
  deriving (Show)

instance Monoid (Bag a) where
    mempty = Empty
    
    mappend l r = 
      case (l, r) of
        (other, Empty) -> other
        
        (_, _) -> Two l r

instance Semigroup (Bag a)

instance Functor Bag where
    fmap f b =
      case b of
        Empty ->
          Empty
          
        One x ->
          One $ f x
          
        Two l r ->
          Two (fmap f l) (fmap f r)
          
        Many xs ->
          Many $ fmap f xs


instance Foldable Bag where
   -- foldr :: (a -> b -> b) -> b -> t a -> b
   foldr _ z Empty = z
   foldr f z (One x) = f x z
   foldr f z (Two x y) = foldr f (foldr f z y) x
   foldr f z (Many xs) = foldr f z xs

singleton :: a -> Bag a
singleton =
  One

      
fromList :: [a] -> Bag a
fromList =
  Many
  
  
toList :: Bag a -> [a]
toList bag =
  toList' bag []
  where toList' :: Bag a -> [a] -> [a]
        toList' bag list =
          case bag of
            Empty ->
              list
            
            One x ->
              x : list
              
            Two left right ->
              toList' left (toList' right list)
              
            Many xs ->
              xs ++ list