module Language.Hawk.Data.Bag where

import Prelude hiding (map)

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