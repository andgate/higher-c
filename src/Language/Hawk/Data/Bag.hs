module Language.Hawk.Data.Bag where

import Prelude hiding (map)

data Bag a 
  = Empty
  | One a
  | Two (Bag a) (Bag a)
  | Many [a]
  
empty :: Bag a
empty = Empty

singleton :: a -> Bag a
singleton = One

cons :: a -> Bag a -> Bag a
cons x bag =
  case bag of
    Empty ->
      One x
    
    _ ->
      Two (One x) bag


append :: Bag a -> Bag a -> Bag a
append left right =
  case (left, right) of
    (other, Empty) ->
      other
      
    (_, _) ->
      Two left right
      

map :: (a -> b) -> Bag a -> Bag b
map f bag =
  case bag of
    Empty ->
      Empty
      
    One x ->
      One $ f x
      
    Two left right ->
      Two (map f left) (map f right)
      
    Many xs ->
      Many $ fmap f xs
      
      
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