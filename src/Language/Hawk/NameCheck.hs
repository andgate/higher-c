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
import Data.Map (Map)
import Data.Text (Text)

import Language.Hawk.NameCheck.Environment (Env)
import Language.Hawk.NameCheck.Error
import Language.Hawk.NameCheck.Message
import Language.Hawk.NameCheck.State
import Language.Hawk.Syntax

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Language.Hawk.NameCheck.Environment as Env



namecheck :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
             , MonadChronicle (Bag e) m, AsNcErr e
             ) => Map FilePath [Decl] -> m (Map FilePath [Decl])
namecheck ds = do
  let exName (Def n _) ns = n:ns
      exName _         ns = ns
  
      exExp (Def n e) es = e:es
      exExp _         es = es

      env = Env.fromList $ foldr exName [] $ concat $ Map.elems ds
      es = foldr exExp [] $ concat $ Map.elems ds

  mapM_ (validate env) es 
  return ds


validate :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
             , MonadChronicle (Bag e) m, AsNcErr e
             ) => Env -> Exp -> m  ()
validate env = \case
  e@(EVar n) ->
    if env `Env.check` n 
      then return ()
      else confess $ One (_UndeclaredNameFound # (n, e))

  EApp e1 e2 -> do
    validate env e1
    validate env e2

  ELam n e -> do
    let env' = Env.insert n (Env.push env)
    validate env' e

  ELet (n, e1) e2 -> do
    validate env e1
    let env' = Env.insert n (Env.push env)
    validate env' e2

  ELit _ -> return () -- Literals cannot contain names

  e@(ECon n) ->
    if env `Env.check` n
       then return ()
       else confess $ One (_UndeclaredNameFound # (n, e))
            
  EPrim _ -> return () -- Primitive instructions cannot contain names

  EIf e1 e2 e3 -> do
    validate env e1
    validate env e2
    validate env e3


  EDup e -> validate env e

  EFree n e ->
    let env' = Env.delete n env
    in validate env' e

  EType _ e -> validate env e

  ETLit _ e -> validate env e
  ELoc _ e -> validate env e

  EParen e -> validate env e
