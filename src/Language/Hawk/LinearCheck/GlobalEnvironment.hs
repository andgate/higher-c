{-# LANGUAGE  TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.LinearCheck.GlobalEnvironment where

import Control.Lens
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set


-------------------------------------------------------------------------------
-- Global Environment for Linearity Checker
-------------------------------------------------------------------------------

data GlobalEnv
  = GlobalEnv
    { _globalVars :: Set Text
    }
  deriving (Eq, Show)


makeClassy ''GlobalEnv


instance Monoid GlobalEnv where
    mempty = empty
    mappend = merge


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

empty :: GlobalEnv
empty = GlobalEnv Set.empty


lookup :: HasGlobalEnv e => Text -> e -> Bool
lookup n env
  = Set.member n (env^.globalEnv.globalVars)
  

merge :: HasGlobalEnv e => e -> e -> e
merge e1 e2 =
  e1 & globalEnv . globalVars %~ Set.union (e2^.globalEnv.globalVars)

mergeMany :: [GlobalEnv] -> GlobalEnv
mergeMany = foldr merge empty


mergeSome :: HasGlobalEnv e => [e] -> e
mergeSome = foldr1 merge


singleton :: Text -> GlobalEnv
singleton n =
  GlobalEnv { _globalVars = Set.singleton n } 

    
fromList :: [Text] -> GlobalEnv
fromList ns = 
  GlobalEnv { _globalVars = Set.fromList ns } 

toList :: HasGlobalEnv e => e -> [Text]
toList env =
  Set.toList (env^.globalEnv.globalVars) 
