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
module Language.Hawk.KindsCheck where

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
import Language.Hawk.KindsCheck.Environment (Env)
import Language.Hawk.KindsCheck.Error
import Language.Hawk.KindsCheck.Message
import Language.Hawk.KindsCheck.Result (KcResult (..))
import Language.Hawk.TypeCheck.Result (TcResult, tcSigs, tcDecls)

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T
import qualified Language.Hawk.KindsCheck.Environment as Env
import qualified Language.Hawk.KindsCheck.Result as R
import qualified Language.Hawk.TypeCheck.Result as TcR


-----------------------------------------------------------------------
-- Kinds Checking
-----------------------------------------------------------------------

kindscheck :: ( MonadLog (WithSeverity msg) m, AsKcMsg msg
              , MonadChronicle (Bag e) m, AsKcErr e )
           => TcResult -> m KcResult
kindscheck r = do

  ds' <- mapM (mapM (inferExp Env.empty)) (r^.tcDecls)
  
  logInfo (_KcComplete # ())
  return KcResult { _kcSigs = r^.tcSigs
                  , _kcDecls = ds'
                  }


inferExp :: ( MonadChronicle (Bag e) m, AsKcErr e )
         => Env -> Exp -> m Exp
inferExp env = \case
  EFree n e -> do
    e' <- inferExp env e
    let k = kind e'
    unless (k `ksub` KPop) (confess $ One (_KindMismatch # (k, KPop)))
    return $ EFree n e'

  ELet (n, e1) e2 -> do
    e1' <- inferExp env e1
    e2' <- inferExp env e2
    let k = kind e1'
    unless (k `ksub` KPop) (confess $ One (_KindMismatch # (k, KPop)))
    return $ ELet (n, e1') e2'


  EType t e -> do
    (_, t') <- inferType env t
    e' <- inferExp env e
    return $ EType t' e'    
    
  e -> return e



inferType :: (MonadChronicle (Bag e) m, AsKcErr e)
      => Env -> Type -> m (Kind, Type)
inferType env = \case
  t@(TVar n) ->
    case Env.lookup n env of
      Nothing -> confess $ One (_KindUnknown # n)
      Just k  -> return (k, TKind k t)


  t@(TCon n) ->
    case Env.lookup n env of
      Nothing -> confess $ One (_KindUnknown # n)
      Just k  -> return (k, TKind k t)


  t@(TApp t1 t2) -> do
    (k1, t1') <- inferType env t1
    (k2, t2') <- inferType env t2
    case k1 of
      KArr a b ->
        if a `ksub` k2
           then return (b, TKind b $ TApp t1' t2')
           else confess $ One (_KindMismatch # (a, k2))
      _ -> confess $ One (_KindArrowExpected # k1)


  t@(TArr t1 t2) -> do
    (k1, t1') <- inferType env t1
    (k2, t2') <- inferType env t2
    unless (k1 `ksub` KStar) (confess $ One (_KindMismatch # (k1, KStar)))
    unless (k2 `ksub` KPop)  (confess $ One (_KindMismatch # (k1, KPop)))
    return (k2, TKind k2 $ TArr t1' t2')


  t@(TLoli t1 t2) -> do
    (k1, t1') <- inferType env t1
    (k2, t2') <- inferType env t2
    unless (k1 `ksub` KPop) (confess $ One (_KindMismatch # (k1, KPop)))
    unless (k2 `ksub` KPop) (confess $ One (_KindMismatch # (k1, KPop)))
    return (k2, TKind k2 $ TLoli t1' t2')


  TKind k t -> do
    (k', t') <- inferType env t
    if k' `ksub` k
      then return (k, TKind k t')
      else confess $ One (_KindMismatch # (k', k))

  TLoc l t -> do
    -- push location onto a location stack, for error reporting
    (k, t') <- inferType env t
    return (k, TLoc l t')


  TParen t -> do
    (k, t') <- inferType env t
    return (k, TKind k $ TParen t')
