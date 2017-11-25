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
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding ((<>))
import Data.Set (Set, (\\))
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Syntax
import Language.Hawk.KindsCheck.Result (KcResult, kcSigs, kcDecls)
import Language.Hawk.LinearCheck.Error
import Language.Hawk.LinearCheck.GlobalEnvironment (GlobalEnv, HasGlobalEnv, globalEnv, globalVars)
import Language.Hawk.LinearCheck.LocalEnvironment (LocalEnv, HasLocalEnv, localEnv, envLin, envReg)
import Language.Hawk.LinearCheck.Message
import Language.Hawk.LinearCheck.State
import Language.Hawk.LinearCheck.Result (LcResult (..))


import qualified Data.Map.Strict as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T
import qualified Language.Hawk.LinearCheck.LocalEnvironment as LEnv
import qualified Language.Hawk.LinearCheck.GlobalEnvironment as GEnv


------------------------------------------------------------------------
-- Linearity Check
-----------------------------------------------------------------------

linearcheck :: ( MonadLog (WithSeverity msg) m, AsLcMsg msg
               , MonadChronicle (Bag e) m, AsLcErr e )
            => KcResult -> m LcResult
linearcheck r = do
  -- Generate global environment
  let genv = GEnv.fromList $ Map.keys (r^.kcDecls)
  ds' <- mapM (mapM $ runCheck genv) (r^.kcDecls)
  logInfo (_LcComplete # ())
  return LcResult { _lcSigs = r^.kcSigs
                  , _lcDecls = ds'
                  }

runCheck :: ( MonadChronicle (Bag e) m, AsLcErr e )
         => GlobalEnv -> Exp -> m Exp
runCheck genv e =
  fst <$> runReaderT (checkExp LEnv.empty e) genv  


checkExp :: ( MonadReader r m, HasGlobalEnv r
            , MonadChronicle (Bag e) m, AsLcErr e )
         => LocalEnv -> Exp -> m (Exp, LocalEnv)
checkExp env = \case
  e@(EVar n) ->
    case LEnv.lookup n env of
      Just (LEnv.LinCtx 0)  -> confess $ One (_LcPreviouslyConsumed # n)
      Just (LEnv.LinCtx _)  -> return (e, LEnv.free n env)
      Just LEnv.RegCtx -> return (e, env)
      Nothing -> do
        isGlobal <- views globalEnv (GEnv.lookup n)
        unless isGlobal
               $ error "Variable not registered in context."   -- how was this reached!?
        return (e, env)


  EApp e1 e2 -> do
    (e1', env1) <- checkExp env e1
    (e2', env2)  <- checkExp env1 e2
    return (EApp e1' e2', env2)


  ELam n e -> do
    let env1 = LEnv.extendLinear n env
    (e', env2) <- checkExp env1 e
    unless (LEnv.isConsumed n env2)
           $ confess $ One (_LcLamUnconsumed # n)
    return (ELam n e', env2)


  ELet (n, e1) e2 -> do
    (e1', env1) <- checkExp env e1
    let env2 = LEnv.extendLinear n env1
    (e2', env3) <- checkExp env2 e2
    unless (LEnv.isConsumed n env3)
           $ confess $ One (_LcLetUnconsumed # n)
    return (ELet (n, e1') e2', env3)


  e@(ELit _) -> return (e, env)
  e@(ECon _) -> return (e, env)
  e@(EPrim _) -> return (e, env)

  EIf e1 e2 e3 -> do
    (e1', env1) <- checkExp env e1
    (e2', env2) <- checkExp env1 e2
    (e3', env3) <- checkExp env1 e3
    
    let diff = Map.differenceWith f (env2^.localEnv.envLin) (env3^.localEnv.envLin)
        f a b = if a == b then Nothing else Just a
        
    unless (Map.null diff)
           $ confess $ One (_LcBranchMismatch # Map.keys diff )
    return (EIf e1' e2' e3', env3)

    
  EDup n -> do
    when (LEnv.isConsumed n env)
         $ confess $ One (_LcPreviouslyConsumed # n)
    return (EDup n, env)

    
  EFree n e -> do
    let env1 = LEnv.free n env
    (e', env2) <- checkExp env1 e
    return (EFree n e', env2)
    

  EType t e -> do
    (e', env') <- checkExp env e
    return (EType t e', env')

  ETLit tl e -> do
    (e', env') <- checkExp env e
    return (ETLit tl e', env')

  ELoc l e -> do
    (e', env') <- checkExp env e
    return (ELoc l e', env')

  EParen e -> do
    (e', env') <- checkExp env e
    return (EParen e', env')
