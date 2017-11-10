
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
import Data.Binary
import Data.Foldable
import Data.Map.Strict (Map)
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


import qualified Data.ByteString       as BS
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
  condemn $
    loadFiles (_hkcSrcFiles conf)
      >>= lexer
      >>= dumpLx conf
      >>= parseMany

  return ()




dumpLx :: ( MonadIO m, Binary a)
        => HkcConfig -> Map FilePath a -> m (Map FilePath a)
dumpLx conf t = do
  when (conf^.hkcDumpLxBin) (dumpBinaries (conf^.hkcBuildDir) t)
  return t


dumpBinaries :: (MonadIO m, Binary a)
           => FilePath -> Map FilePath a -> m ()
dumpBinaries dir =
  mapM_ (dumpBinary dir) . Map.toList


dumpBinary :: (MonadIO m, Binary a)
           => FilePath -> (FilePath, a) -> m (FilePath, a)
dumpBinary dir i@(fp, o) = do
  liftIO $ encodeFile fp o
  return i
  where
    fp' = undefined           

