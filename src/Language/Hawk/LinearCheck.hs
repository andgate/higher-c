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
import Language.Hawk.LinearCheck.Error
import Language.Hawk.LinearCheck.GlobalEnvironment (GlobalEnv, HasGlobalEnv, globalEnv, globalVars)
import Language.Hawk.LinearCheck.LocalEnvironment (LocalEnv, HasLocalEnv, localEnv, envLin, envReg)
import Language.Hawk.LinearCheck.Message
import Language.Hawk.LinearCheck.State


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
            => Image -> m Image
linearcheck img = do
  -- Generate global environment
  let genv1 = GEnv.fromList $ map readName (img^..imgFns.traversed.fnName)
      genv2 = GEnv.fromList $ concatMap structNames (img^.imgTStructs)
      genv = genv1 <> genv2
  
      
  logInfo (_LcBegin # ())
  img' <- condemn $
    mapMOf (imgFns.each) (runCheck genv) img
  logInfo (_LcComplete # ())
  return img'

runCheck :: ( MonadChronicle (Bag e) m, AsLcErr e )
         => GlobalEnv -> Fn -> m Fn
runCheck genv (Fn n ps e) = do
  let ns = Set.toList $ fv ps
      lenv1 = foldr LEnv.extendLinear LEnv.empty ns

  (e', lenv2) <- runReaderT (checkExp (lenv1, locExp e) e) genv
  unless (LEnv.areTheseConsumed ns lenv2)
         $ confess $ One (_LcParamsUnconsumed # (ns, locName' n))
  return $ Fn n ps e'


checkExp :: ( MonadReader r m, HasGlobalEnv r
            , MonadChronicle (Bag e) m, AsLcErr e )
         => (LocalEnv, Loc) -> Exp -> m (Exp, LocalEnv)
checkExp (env, l) = \case
  e@(EVar n) ->
    case LEnv.lookup n env of
      Just (LEnv.LinCtx 0)  -> confess $ One (_LcPreviouslyConsumed # (n, l))
      Just (LEnv.LinCtx _)  -> return (e, LEnv.free n env)
      Just LEnv.RegCtx -> return (e, env)
      Nothing -> do
        isGlobal <- views globalEnv (GEnv.lookup n)
        unless isGlobal
               $ error $ show l ++ "\n\t\tVariable not registered in context."   -- how was this reached!?
        return (e, env)


  EApp f x -> do
    (f', env1) <- checkExp (env, l) f
    (x', env2) <- checkExp (env1, l) x
    return (EApp f' x', env2)


  ELam n e -> do
    let n' = readName n
        env1 = LEnv.extendLinear n' env
    (e', env2) <- checkExp (env1, l) e

    unless (LEnv.isConsumed n' env2)
           $ confess $ One (_LcLetUnconsumed # (n', l))
      
    return (ELam n e', env2)


  ELet (n, e1) e2 -> do
    (e1', env1) <- checkExp (env, l) e1
    let n' = readName n
        env1' = LEnv.extendLinear n' env1
    (e2', env2) <- checkExp (env1', l) e2

    unless (LEnv.isConsumed n' env2)
           $ confess $ One (_LcLetUnconsumed # (n', l))
    
    return (ELet (n, e1') e2', env2)


  e@(ELit _) -> return (e, env)
  e@(ECon _) -> return (e, env)
  
  EPrim i e1 e2 -> do
    (e1', env1) <- checkExp (env, l) e1
    (e2', env2) <- checkExp (env1, l) e2
    return (EPrim i e1' e2', env2)

  EIf e1 e2 e3 -> do
    (e1', env1) <- checkExp (env, l) e1
    (e2', env2) <- checkExp (env1, l) e2
    (e3', env3) <- checkExp (env1, l) e3
    
    let diff = Map.differenceWith f (env2^.localEnv.envLin) (env3^.localEnv.envLin)
        f a b = if a == b then Nothing else Just a
        
    unless (Map.null diff)
           $ confess $ One (_LcBranchMismatch # (Map.keys diff, l) )
    
    return (EIf e1' e2' e3', env3)

    
  EDup n -> do
    let n' = readName n
    
    when (LEnv.isConsumed n' env)
         $ confess $ One (_LcPreviouslyConsumed # (n', l))

    return (EDup n, env)

    
  EFree ns e -> do
    let env1 = foldr LEnv.free env (map readName ns)
    (e', env2) <- checkExp (env1, l) e
    return (EFree ns e', env2)
    

  EType t e -> do
    (e', env') <- checkExp (env, l) e
    return (EType t e', env')

  ELoc l' e -> do
    (e', env') <- checkExp (env, l') e
    return (ELoc l' e', env')

  EParen e -> do
    (e', env') <- checkExp (env, l) e
    return (EParen e', env')
