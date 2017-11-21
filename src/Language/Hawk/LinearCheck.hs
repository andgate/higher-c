{-# LANGUAGE  OverloadedStrings
            , FlexibleInstances
            , FlexibleContexts
            , GADTs
            , LambdaCase
            , MultiParamTypeClasses
            , FunctionalDependencies
            , RankNTypes
            , TemplateHaskell
            , GeneralizedNewtypeDeriving
            , TypeSynonymInstances
            , UndecidableInstances
            , StandaloneDeriving
  #-}
module Language.Hawk.LinearCheck where

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control

import Data.Bag
import Data.Default.Class
import Data.List (lookup, union, concatMap, nub, find, delete, intersect)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding ((<>))
import Data.Set (Set, (\\))
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Syntax
import Language.Hawk.KindsCheck.Result (KcResult, kcSigs, kcDecls)
import Language.Hawk.LinearCheck.Environment (LcEnv)
import Language.Hawk.LinearCheck.Error
import Language.Hawk.LinearCheck.Message
import Language.Hawk.LinearCheck.State
import Language.Hawk.LinearCheck.Result (LcResult (..))


import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T
import qualified Language.Hawk.LinearCheck.Environment as Env


-----------------------------------------------------------------------
-- Linearity Check
-----------------------------------------------------------------------

linearcheck :: ( MonadLog (WithSeverity msg) m, AsLcMsg msg
               , MonadChronicle (Bag e) m, AsLcErr e )
            => KcResult -> m LcResult
linearcheck r = do
  ds' <- mapM (mapM $ checkExp Env.empty) (r^.kcDecls)
  logInfo (_LcComplete # ())
  return LcResult { _lcSigs = r^.kcSigs
                  , _lcDecls = ds'
                  }


checkExp :: ( MonadChronicle (Bag e) m, AsLcErr e )
         => LcEnv -> Exp -> m Exp
checkExp env = \case
  EVar n ->
    case Env.lookup n env of
      Just Env.RegCtx -> return $ EVar n
        
      Just Env.LinCtx -> let env' = Env.use env n
                         in return $ EVar n -- linear
                            
      Nothing -> error "unbound variable encountered?"


  ELet (n, e1) e2 -> do
    e1' <- checkExp env e1

    -- get type of e1'
    let env' = Env.extendLinear env n
    e2' <- checkExp env' e2
    return $ ELet (n, e1') e2'

    
  e -> return e
