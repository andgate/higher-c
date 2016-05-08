module Language.Hawk.Report.Result where

import Control.Monad.Except (Except, runExcept)


import qualified Language.Hawk.Data.Bag as Bag
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


-- | Result types

data Result info warning error result
  = Result
    { _info :: info
    , _warnings :: LBag warning
    , _answer :: Answer (LBag error) result
    }
    

data Answer e a = Ok a | Err e


type LBag a
  = Bag.Bag (A.Located a)
  

data One a = None | One a


-- | Result Helpers

ok :: (Monoid i) => a -> Result i w e a
ok value =
  Result mempty Bag.empty (Ok value)
  
throw :: (Monoid i) => R.Region -> e -> Result i w e a
throw region err =
  Result mempty Bag.empty (Err (Bag.singleton (A.A region err)))
  
  
throwMany :: (Monoid i) => [A.Located e] -> Result i w e a
throwMany errors =
  Result mempty Bag.empty (Err (Bag.fromList errors))
  
accumulate :: (Monoid i) => i -> a -> Result i w e a
accumulate info value =
  Result info Bag.empty (Ok value)
  
warn :: (Monoid i) => R.Region -> w -> a -> Result i w e a
warn region warning a =
  Result mempty (Bag.singleton (A.A region warning)) (Ok a)
  

-- | Extraction helpers
from :: (Monoid i) => (e -> e') -> Except [A.Located e] a -> Result i w e' a
from f except =
  case runExcept except of
    Right answer ->
      ok answer
    
    Left errors ->
      throwMany (map (A.map f) errors)
      

mapError :: (e -> e') -> Result i w e a -> Result i w e' a
mapError f (Result info warnings rawResult) =
  Result info warnings $
    case rawResult of
      Ok v ->
        Ok v
      
      Err msgs ->
        Err (Bag.map (A.map f) msgs)
        
        
format :: (Monoid i) => (e -> e') -> Result () w e a -> Result i w e' a
format f (Result _ warnings answer) =
  mapError f (Result mempty warnings answer)
  
  
answerToEither :: (A.Located e -> e') -> (a -> a') -> Answer (LBag e) a -> Either [e'] a'
answerToEither onErr onOk answer =
  case answer of
    Ok value ->
      Right (onOk value)
      
    Err errors ->
      Left (Bag.toList . Bag.map onErr $ errors)
      

oneToValue :: b -> (a -> b) -> One a -> b
oneToValue fallback onOne one =
  case one of
    None -> 
      fallback
    
    One value ->
      onOne value
      

-- | Functor Instance for Result
instance Functor (Result i w e) where
  fmap f (Result info warnings rawResult) =
    case rawResult of
      Ok a ->
        Result info warnings (Ok $ f a)
        
      Err msgs ->
        Result info warnings (Err msgs)

-- | Applicative Instance for Result
instance (Monoid i) => Applicative (Result i w e) where
  pure value =
    ok value
    
  (<*>) (Result info warnings resultFunc) (Result info' warnings' resultVal) =
    Result (mappend info info') (Bag.append warnings warnings') $
      case (resultFunc, resultVal) of
        (Ok f, Ok val) ->
          Ok (f val)
          
        (Err msgs, Err msgs') ->
          Err (Bag.append msgs msgs')
          
        (Err msgs, _) ->
          Err msgs
          
        (_, Err msgs) ->
          Err msgs

-- | Monad instance for Result        
instance (Monoid i) => Monad (Result i w e) where
  return value =
    ok value
    
  (>>=) (Result info warnings rawResult) callback =
    case rawResult of
      Err msg ->
        Result info warnings (Err msg)
        
      Ok value ->
        let
          (Result info' warnings' rawResult') =
            callback value
        in
          Result
            (mappend info info')
            (Bag.append warnings warnings')
            rawResult'
            
            
instance Monoid (One a) where
  mempty =
    None
    
  mappend left right =
    case (left, right) of
      (other, None) ->
        other
        
      (None, other) ->
        other
        
      (One _, One _) ->
        error "There can be only one!"