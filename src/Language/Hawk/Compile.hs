
{-# LANGUAGE RankNTypes
           , OverloadedStrings
           , FlexibleContexts
           , AllowAmbiguousTypes
  #-}
module Language.Hawk.Compile
        ( hkc
        , module Language.Hawk.Compile.Config
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
import Data.Maybe (isJust)

import Language.Hawk.Load
import Language.Hawk.Parse
import Language.Hawk.Lex.Token
import Language.Hawk.Lex
import Language.Hawk.Syntax

import Language.Hawk.Compile.Config
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Language.Hawk.Compile.Monad
import Text.PrettyPrint.Leijen.Text (pretty)


import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified Data.Map.Strict       as Map
import qualified Language.Hawk.Parse   as P
import qualified Language.Hawk.TypeCheck as Tc
import qualified Language.Hawk.TypeCheck.Environment as TcEnv
import qualified Language.Hawk.NameCheck as Nc
import qualified Language.Hawk.NameCheck.Environment as NcEnv


hkc :: HkcConfig -> IO ()
hkc cfg = runHkc (compile cfg)


compile
  :: ( MonadLog (WithSeverity msg) m, AsHkcMsg msg, AsLdMsg msg, AsLxMsg msg, AsPsMsg msg, AsNcMsg msg, AsTcMsg msg
     , MonadChronicle (Bag e) m, AsHkcErr e, AsLdErr e, AsPsErr e, AsLxErr e , AsNcErr e, AsTcErr e
     , MonadIO m
     )
  => HkcConfig -> m ()
compile conf = do
  condemn $ do
    txts <- loadFiles (_hkcSrcFiles conf)

    tks <- concat <$> mapM lexer txts
    when (isJust $ _hkcDumpLx conf)
         (dumpLx tks)  

    ps <- mapM parse tks
    when (isJust $ _hkcDumpPs conf)
         (dumpPs ps)
  
  return ()

dumpLx :: MonadIO m => [(FilePath, [Token])] -> m ()
dumpLx tks =
  return ()


dumpPs :: MonadIO m => [(FilePath, Decl)] -> m ()
dumpPs ps =
  return ()
