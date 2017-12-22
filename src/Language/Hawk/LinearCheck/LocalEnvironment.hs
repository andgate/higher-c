{-# LANGUAGE  TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.LinearCheck.LocalEnvironment where

import Control.Lens
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set


-------------------------------------------------------------------------------
-- Linear Checking Environment
-------------------------------------------------------------------------------

data LocalEnv
  = LocalEnv
    { _envReg    :: Set Text
    , _envLin    :: Map Text Int
    }
  deriving (Eq, Show)


makeClassy ''LocalEnv


instance Monoid LocalEnv where
    mempty = empty
    mappend = merge


data TyCtx
  = RegCtx | LinCtx Int
  deriving(Show, Eq)


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

empty :: LocalEnv
empty = LocalEnv Set.empty Map.empty


extendIntuit :: HasLocalEnv e => Text -> e -> e
extendIntuit n env =
  env & localEnv . envReg %~ Set.insert n

  
extendLinear :: HasLocalEnv e => Text -> e -> e
extendLinear n env =
  env & localEnv . envLin %~ Map.insert n 1


removeReg :: HasLocalEnv e => Text -> e -> e
removeReg n env =
  env & localEnv . envReg %~ Set.delete n


removeLinear :: HasLocalEnv e => Text -> e -> e
removeLinear n env =
  env & localEnv . envLin %~ Map.delete n


free :: HasLocalEnv e => Text -> e -> e
free n env =
  env & localEnv . envLin . at n . _Just %~ pred

duplicate :: HasLocalEnv e => Text -> e -> e
duplicate n env =
  env & localEnv . envLin . at n . _Just %~ succ 


lookup :: HasLocalEnv e => Text -> e -> Maybe TyCtx
lookup n env
  | Set.member n (env^.localEnv.envReg) = Just RegCtx
  | otherwise =
      LinCtx <$> Map.lookup n (env^.localEnv.envLin) 
      

isConsumed :: HasLocalEnv e => Text -> e -> Bool
isConsumed n env =
  case Map.lookup n (env^.localEnv.envLin) of
    Just 0 -> True
    Just _ -> False
    Nothing -> False


areTheseConsumed :: HasLocalEnv e => [Text] -> e -> Bool
areTheseConsumed ns env =
  all (`isConsumed` env) ns 

merge :: HasLocalEnv e => e -> e -> e
merge e1 e2 =
  e1 & localEnv . envReg %~ Set.union (e2^.localEnv.envReg)
     & localEnv . envLin %~ Map.unionWith (+) (e2^.localEnv.envLin)


mergeMany :: [LocalEnv] -> LocalEnv
mergeMany = foldr merge empty


mergeSome :: HasLocalEnv e => [e] -> e
mergeSome = foldr1 merge
