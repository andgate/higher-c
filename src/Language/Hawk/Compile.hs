
{-# LANGUAGE RankNTypes
           , OverloadedStrings
           , FlexibleContexts
           , AllowAmbiguousTypes
  #-}
module Language.Hawk.Compile
        ( hkc
        , module Language.Hawk.Compile.Config
        ) where


import Control.Exception
import Control.Lens hiding ((<.>))
import Control.Monad.Chronicle
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Control
import Data.Aeson (ToJSON, FromJSON)
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
import System.FilePath ((</>), (<.>), takeBaseName)
import System.IO (hClose, openFile, IOMode(..))


import qualified Data.Aeson            as JSN
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString       as BS
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Data.Vector           as V
import qualified Data.Map.Strict       as Map
import qualified Language.Hawk.Parse   as P
import qualified Language.Hawk.TypeCheck as Tc
import qualified Language.Hawk.TypeCheck.Environment as TcEnv
import qualified Language.Hawk.NameCheck as Nc
import qualified Language.Hawk.NameCheck.Environment as NcEnv
import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Yaml as Y

hkc :: HkcConfig -> IO ()
hkc cfg = runHkc (compile cfg)


compile
  :: ( MonadLog (WithSeverity msg) m, AsHkcMsg msg, AsLdMsg msg, AsLxMsg msg, AsPsMsg msg, AsNcMsg msg, AsTcMsg msg
     , MonadChronicle (Bag e) m, AsHkcErr e, AsLdErr e, AsPsErr e, AsLxErr e , AsNcErr e, AsTcErr e
     , MonadIO m, HasHkcConfig c
     )
  => c -> m ()
compile conf = do
  condemn $
    loadFiles (conf^.hkcSrcFiles)
      >>= lexMany    >>= dumpLx conf
      >>= parseMany  >>= dumpPs conf

  return ()

dumpLx :: ( MonadIO m, HasHkcConfig c
          , Binary a, PP.Pretty a, ToJSON a )
       => c -> Map FilePath a -> m (Map FilePath a)
dumpLx conf t = do
  let odir = (conf^.hkcBuildDir) </> "lex"
  when (conf^.hkcDumpLxPretty) (dumpPretties odir t)
  when (conf^.hkcDumpLxBin)    (dumpBinaries odir t)
  when (conf^.hkcDumpLxJson)   (dumpJsons odir t)
  when (conf^.hkcDumpLxYaml)   (dumpYamls odir t)
  return t


dumpPs :: ( MonadIO m, HasHkcConfig c
          , Binary a, PP.Pretty a, ToJSON a )
       => c -> Map FilePath a -> m (Map FilePath a)
dumpPs conf t = do
  let odir = (conf^.hkcBuildDir) </> "parse"
  when (conf^.hkcDumpPsPretty) (dumpPretties odir t)
  when (conf^.hkcDumpPsBin)    (dumpBinaries odir t)
  when (conf^.hkcDumpPsJson)   (dumpJsons odir t)
  when (conf^.hkcDumpPsYaml)   (dumpYamls odir t)
  return t



dumpPretties :: (MonadIO m, PP.Pretty a)
             => FilePath -> Map FilePath a -> m ()
dumpPretties odir =
  mapM_ (dumpPretty odir) . Map.toList


dumpBinaries :: (MonadIO m, Binary a)
             => FilePath -> Map FilePath a -> m ()
dumpBinaries odir =
  mapM_ (dumpBinary odir) . Map.toList


dumpJsons :: (MonadIO m, ToJSON a)
           => FilePath -> Map FilePath a -> m ()
dumpJsons odir =
  mapM_ (dumpJson odir) . Map.toList


dumpYamls :: (MonadIO m, ToJSON a)
           => FilePath -> Map FilePath a -> m ()
dumpYamls odir =
  mapM_ (dumpYaml odir) . Map.toList
  

dumpPretty :: (MonadIO m, PP.Pretty a)
           => FilePath -> (FilePath, a) -> m (FilePath, a)
dumpPretty odir i@(fp, o) = do
  let fp' = odir </> takeBaseName fp <.> "txt"
  liftIO $ bracket
    (openFile fp' WriteMode) hClose
    (\h -> PP.hPutDoc h (PP.pretty o))
    
  return i


dumpBinary :: (MonadIO m, Binary a)
           => FilePath -> (FilePath, a) -> m (FilePath, a)
dumpBinary odir i@(fp, o) = do
  let fp' = odir </> takeBaseName fp 
  liftIO $ encodeFile fp' o
  return i


dumpJson :: (MonadIO m, ToJSON a)
         => FilePath -> (FilePath, a) -> m (FilePath, a)
dumpJson odir i@(fp, o) = do
  let fp' = odir </> takeBaseName fp <.> ".json"
  liftIO $ LBS.writeFile fp' (JSN.encode o)
  return i


dumpYaml :: (MonadIO m, ToJSON a)
         => FilePath -> (FilePath, a) -> m (FilePath, a)
dumpYaml odir i@(fp, o) = do
  let fp' = odir </> takeBaseName fp <.> ".yaml"
  liftIO $ Y.encodeFile fp' o
  return i
