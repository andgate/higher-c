module Language.Hawk.Report.Result where

import Control.Applicative
import Control.Monad.Except (Except, runExcept)
import Data.Monoid
import Language.Hawk.Compile.Options
import Language.Hawk.Data.Bag
import Language.Hawk.Report.Error
import Language.Hawk.Report.Info
import Language.Hawk.Report.Report
import Language.Hawk.Report.Warning


-- | Result types

data Result r
  = Result 
   { _info :: Bag Info
   , _warnings :: Bag Warning
   , _answer :: Either (Bag Error) r
   } deriving (Show)


resultReports :: Opts -> Result a -> [Report]
resultReports o (Result i w a) = 
     reportFilter o i 
  ++ reportFilter o w
  ++ case a of
        Left errs -> toReport <$> (toList errs)
        Right _ -> []


reportFilter :: (HasVerbosity r, HasFlags r, Reportable r) => Opts -> Bag r -> [Report]
reportFilter o = fmap toReport . messageFilter o .  toList

getAnswer :: Result a -> Maybe a
getAnswer (Result _ _ a) =
  case a of
    Left _ -> Nothing
    Right v -> Just v 

throw :: Error -> Result a
throw err =
  Result mempty mempty (Left $ singleton err)


throwMany :: [Error] -> Result a
throwMany errors =
  Result mempty mempty (Left $ fromList errors)


info :: Info -> Result ()
info i =
  Result (singleton i) mempty (Right ())


warn :: Warning -> Result ()
warn w =
  Result mempty (singleton w) (Right ())


    
  


instance Functor Result where
  fmap f (Result i w a) =
      Result i w (f <$> a)


instance Applicative Result where
  pure r =
      Result mempty mempty (Right r)

  (Result i w a) *> (Result i' w' a') =
      Result
        (i <> i')
        (w <> w')
        $ case (a, a') of
            (Right _, Right r') ->
                Right r'
            
            (Left errs, Left errs') -> 
                Left (errs <> errs')
          
            (Left errs, _) ->
                Left errs
            
            (_, Left errs') ->
                Left errs'

  (<*>) (Result i w af) (Result i' w' av) =
      Result
        (i <> i')
        (w <> w')
        $ case (af, av) of
            (Right f, Right r) ->
                Right $ f r

            (Left errs, Left errs') ->
                Left (errs <> errs')

            (Left errs, _) ->
                Left errs

            (_, Left errs) ->
                Left errs

instance Monad Result where
  return r =
      Result mempty mempty (Right r)

  (>>) = (*>)

  (>>=) (Result i w a) k =
      case a of
          Left errs ->
              Result i w (Left errs)

          Right r ->
              let
                (Result i' w' a') = k r
              in
                Result (i <> i') (w <> w') a'