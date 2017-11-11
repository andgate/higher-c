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

  mapM_ (validate (env,mempty)) es
  return ds


validate :: ( MonadLog (WithSeverity msg) m, AsNcMsg msg
            , MonadChronicle (Bag e) m, AsNcErr e
            ) => (Env, Loc) -> Exp -> m  ()
validate s@(env, l) = \case
  EVar n ->
    if env `Env.check` n 
      then return ()
      else confess $ One (_UndeclaredNameFound # (n, l))

  EApp e1 e2 -> do
    validate s e1
    validate s e2

  ELam n e -> do
    let env' = Env.insert n (Env.push env)
    validate (env', l) e

  ELet (n, e1) e2 -> do
    validate s e1
    let env' = Env.insert n (Env.push env)
    validate (env', l) e2

  ELit _ -> return () -- Literals cannot contain names

  ECon n ->
    if env `Env.check` n
       then return ()
       else confess $ One (_UndeclaredNameFound # (n, l))
            
  EPrim _ -> return () -- Primitive instructions cannot contain names

  EIf e1 e2 e3 -> do
    validate s e1
    validate s e2
    validate s e3


  EDup e -> validate s e

  EFree n -> validate s e

  EType _ e -> validate s e

  ETLit _ e -> validate s e
  ELoc l' e -> validate (env, l') e

  EParen e -> validate s e
