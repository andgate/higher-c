{-# LANGUAGE RankNTypes
           , OverloadedStrings
           , FlexibleContexts
           , AllowAmbiguousTypes
  #-}
module Language.Hawk.Compile
        ( hkc
        , HkcConfig(..)
        , HkcProduct(..)
        ) where

import Control.Lens
import Control.Monad.Chronicle
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Control
import Data.Bag
import Data.Foldable

import Language.Hawk.Load
import Language.Hawk.Parse
import Language.Hawk.Parse.Lexer
import Language.Hawk.Parse.Lexer.Error
import Language.Hawk.Parse.State
import Language.Hawk.Syntax

import Language.Hawk.Compile.Config
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Language.Hawk.Compile.Monad
import Language.Hawk.Compile.State
import Language.Hawk.Compile.Options
import Text.PrettyPrint.Leijen.Text (pretty)


import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified Language.Hawk.Parse              as P


hkc :: HkcConfig -> IO ()
hkc = runHkc compile

compile
  :: ( MonadState s m, HasHkcState s, HasParseState s
     , MonadReader c m , HasHkcConfig c
     , MonadLog (WithSeverity (WithTimestamp msg)) m, AsHkcMsg msg, AsLoadMsg msg, AsParseMsg msg
     , MonadChronicle (Bag (WithTimestamp e)) m
     , AsHkcErr e, AsLoadErr e, AsParseErr e, AsNameCheckError e, AsTcErr e, AsLexErr e
     , MonadIO m, MonadBaseControl IO m
     )
  => m ()
compile = do
  condemn $ do
    load
    lexer
    parse
    
    liftIO . print . pretty =<< use psToks
    liftIO . print =<< use psOps
    liftIO . print . pretty =<< use hkcItems
  
  --namecheck
  --typecheck
  --optimize
  --codegen
  return ()
