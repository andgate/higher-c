{-# LANGUAGE  TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.LinearCheck.Environment where

import Control.Lens
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)
import Language.Hawk.Syntax.Type

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set


-------------------------------------------------------------------------------
-- Linear Checking Environment
-------------------------------------------------------------------------------

data LcEnv
  = LcEnv
    { _envReg :: Set Text
    , _envLin  :: Set Text
    }
  deriving (Eq, Show)


makeClassy ''LcEnv


instance Monoid LcEnv where
    mempty = empty
    mappend = merge


data TyCtx
  = RegCtx | LinCtx
  deriving(Show, Eq)


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

empty :: LcEnv
empty = LcEnv Set.empty Set.empty


extendIntuit :: HasLcEnv e => e -> Text -> e
extendIntuit env n =
  env & lcEnv . envReg %~ Set.insert n

  
extendLinear :: HasLcEnv e => e -> Text -> e
extendLinear env n =
  env & lcEnv . envLin %~ Set.insert n


remove :: HasLcEnv e => e -> Text -> e
remove env n =
  env & lcEnv . envReg %~ Set.delete n


use :: HasLcEnv e => e -> Text -> e
use env n =
  env & lcEnv . envLin %~ Set.delete n


lookup :: HasLcEnv e => Text -> e -> Maybe TyCtx
lookup n env
  | Set.member n (env^.lcEnv.envReg) = Just RegCtx
  | Set.member n (env^.lcEnv.envLin)  = Just LinCtx
  | otherwise = Nothing
  

merge :: HasLcEnv e => e -> e -> e
merge e1 e2 =
  e1 & lcEnv . envReg %~ Set.union (e2^.lcEnv.envReg)
     & lcEnv . envLin %~ Set.union (e2^.lcEnv.envLin)


mergeMany :: [LcEnv] -> LcEnv
mergeMany = foldr merge empty


mergeSome :: HasLcEnv e => [e] -> e
mergeSome = foldr1 merge


singleton :: Text -> TyCtx -> LcEnv
singleton n = \case
  RegCtx -> empty { _envReg = Set.singleton n } 
  LinCtx -> empty { _envLin = Set.singleton n }

    
fromList :: [(Text, Text)] -> LcEnv
fromList = undefined


toList :: HasLcEnv e => e -> [(Text, Text)]
toList = undefined


