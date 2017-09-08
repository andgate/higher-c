
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
import Language.Hawk.Parse.Lex
import Language.Hawk.Parse.Lex.Error
import Language.Hawk.Syntax

import Language.Hawk.TypeCheck

import Language.Hawk.Compile.Config
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Language.Hawk.Compile.Monad
import Language.Hawk.Compile.State
import Language.Hawk.Compile.Options
import Text.PrettyPrint.Leijen.Text (pretty)


import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified Data.Map.Lazy         as Map
import qualified Language.Hawk.Parse   as P


hkc :: HkcConfig -> IO ()
hkc = runHkc compile

compile
  :: ( MonadState s m, HasHkcState s
     , MonadReader c m , HasHkcConfig c
     , MonadLog (WithSeverity (WithTimestamp msg)) m, AsHkcMsg msg, AsLoadMsg msg, AsParseMsg msg
     , MonadChronicle (Bag (WithTimestamp e)) m
     , AsHkcErr e, AsLoadErr e, AsParseErr e, AsLexErr e, AsNameCheckError e, AsTcErr e
     , MonadIO m, MonadBaseControl IO m
     )
  => m ()
compile = do
  condemn $ do
    load
    xs <- use hkcFileTexts
    docToks <- mapM lexer xs
    forM_ docToks $ \declsToks ->
        forM_ declsToks $ \declToks -> do
            liftIO . print . pretty $ declToks
            parse declToks
              >>= liftIO . print . pretty

  --namecheck

  -- typecheck
    
  --optimize
  --codegen
  return ()
