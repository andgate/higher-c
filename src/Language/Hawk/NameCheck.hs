{-# LANGUAGE  LambdaCase
            , FlexibleContexts
  #-}
module Language.Hawk.NameCheck where

import Control.Lens
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Control

import Data.Bag
import Data.Default.Class
import Data.Map (Map)
import Data.Text (Text)

import Language.Hawk.NameCheck.Environment (Env)
import Language.Hawk.NameCheck.Error
import Language.Hawk.NameCheck.Message
import Language.Hawk.NameCheck.State
import Language.Hawk.Syntax

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Language.Hawk.NameCheck.Environment as Env


-----------------------------------------------------------------------
-- Name Check
-----------------------------------------------------------------------

namecheck :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
             , MonadChronicle (Bag e) m, AsNcErr e
             ) => Image -> m Image
namecheck img = do
  let ns = Set.map readName $ Set.fromList (img^..imgFns.traversed.fnName)
      env = Env.fromSet ns
      
  logInfo (_NcStarted # ns)
  _ <- traverseOf_ (imgFns.each.fnBody) (validate (env,mempty)) img
  logInfo (_NcFinished # ())

  return img

validate :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
            , MonadChronicle (Bag e) m, AsNcErr e
            ) => (Env, Loc) -> Exp -> m Env
validate s@(env, l) = \case
  EVar n -> do
    unless (env `Env.check` n) 
           $ disclose $ One (_UndeclaredNameFound # (n, l))
    return env

  EApp f x -> do
    validate s f
    validate s x
    return env

  ELam n e -> do
    let env' = Env.insert (readName n) env
    validate (env', l) e
    return env

  ELet (n, e1) e2 -> do
    let env' = Env.insert (readName n) env
    validate (env', l) e1
    validate (env', l) e2
    return env

  ELit _ ->
    return env -- Literals cannot contain names

  ECon n -> do
    unless (env `Env.check` n) 
           $ disclose $ One (_UndeclaredNameFound # (n, l))
    return env
            
  EPrim _ ->
    return env -- Primitive instructions cannot contain names

  EIf e1 e2 e3 -> do
    validate s e1
    validate s e2
    validate s e3


  EDup n ->
    validate s (varName n)
    

  EFree ns e ->
    let env' = foldr Env.insert env (map readName ns)
    in validate (env', l) e

  EType _ e -> validate s e
  ELoc l' e -> validate (env, l') e
  EParen e  -> validate s e



validateBinder
  :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
     , MonadChronicle (Bag e) m, AsNcErr e )
  => (Env, Loc) -> Binder -> m Env
validateBinder (env, l) (Binder p e) = do
  env' <- validatePat (env, l) p
  validate (env', l) e


validatePat
  :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
     , MonadChronicle (Bag e) m, AsNcErr e )
  => (Env, Loc) -> Pat -> m Env
validatePat (env, l) = \case
    PVar x ->
      return $ Env.insert x env

    PLoc l' p -> validatePat (env, l') p
