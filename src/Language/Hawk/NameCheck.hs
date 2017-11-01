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
import Data.Text (Text)

import Language.Hawk.NameCheck.Environment (Env)
import Language.Hawk.NameCheck.Error
import Language.Hawk.NameCheck.Message
import Language.Hawk.NameCheck.State
import Language.Hawk.Syntax.Expression

import qualified Data.Text as Text
import qualified Language.Hawk.NameCheck.Environment as Env





namecheck :: ( MonadLog (WithSeverity (WithTimestamp msg)) m, AsNcMsg msg
             , MonadChronicle (Bag (WithTimestamp e)) m, AsNcErr e
             , MonadIO m
             ) => Env -> Exp -> m  ()
namecheck env = \case
  e@(EVar n) ->
    if env `Env.check` n 
      then return ()
      else discloseNow (_UndeclaredNameFound # (n, e))

  EApp e1 e2 -> do
    namecheck env e1
    namecheck env e2

  ELam n e -> do
    let env' = Env.insert n (Env.push env)
    namecheck env' e

  ELet (n, e1) e2 -> do
    namecheck env e1
    let env' = Env.insert n (Env.push env)
    namecheck env' e2

  ELit _ -> return () -- Literals cannot contain names

  e@(ECon n) ->
    if env `Env.check` n
       then return ()
       else discloseNow (_UndeclaredNameFound # (n, e))
            
  EPrim _ -> return () -- Primitive instructions cannot contain names

  EIf e1 e2 e3 -> do
    namecheck env e1
    namecheck env e2
    namecheck env e3


  EDup e -> namecheck env e

  EFree n e ->
    let env' = Env.delete n env
    in namecheck env' e

  EType _ e -> namecheck env e

  ETLit _ e -> namecheck env e
  ELoc _ e -> namecheck env e

  EParen e -> namecheck env e
