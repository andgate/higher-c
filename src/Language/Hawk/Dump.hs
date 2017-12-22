module Language.Hawk.Dump where

import Control.Lens hiding ((<.>))
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Binary

import Language.Hawk.Compile.Config
import Language.Hawk.Load.Result (LdResult)
import Language.Hawk.Lex.Result (LxResult)
import Language.Hawk.Syntax.Image (Image)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeBaseName, takeDirectory)
import System.IO (hClose, openFile, IOMode(..))
import Text.PrettyPrint.Leijen.Text (pretty)

import qualified Data.Aeson            as JSN
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString       as BS
import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Yaml as Y


import qualified Language.Hawk.Syntax.Term.Source as Source


-----------------------------------------------------------------------
-- Data Dumping for Results
-----------------------------------------------------------------------

dumpLx :: ( MonadIO m, HasHkcConfig c )
       => c -> LxResult -> m LxResult
dumpLx conf r = do
  let fp = dumpPath conf "lex" 
  dump fp r ( conf^.hkcDumpLxPretty
            , conf^.hkcDumpLxBin
            , conf^.hkcDumpLxJson
            , conf^.hkcDumpLxYaml
            )
    

dumpPs :: ( MonadIO m, HasHkcConfig c )
       => c -> Image -> m Image
dumpPs conf r = do
  let fp = dumpPath conf "parse"
  dump fp r ( conf^.hkcDumpPsPretty
            , conf^.hkcDumpPsBin
            , conf^.hkcDumpPsJson
            , conf^.hkcDumpPsYaml
            )


dumpNc :: ( MonadIO m, HasHkcConfig c )
       => c -> Image -> m Image
dumpNc conf r = do
  let fp = dumpPath conf "named"
  dump fp r ( conf^.hkcDumpNcPretty
            , conf^.hkcDumpNcBin
            , conf^.hkcDumpNcJson
            , conf^.hkcDumpNcYaml
            )


dumpTc :: ( MonadIO m, HasHkcConfig c )
       => c -> Image -> m Image
dumpTc conf r = do
  let fp = dumpPath conf "typed"
  dump fp r ( conf^.hkcDumpTcPretty
            , conf^.hkcDumpTcBin
            , conf^.hkcDumpTcJson
            , conf^.hkcDumpTcYaml
            )


dumpKc :: ( MonadIO m, HasHkcConfig c )
       => c -> Image -> m Image
dumpKc conf r = do
  let fp = dumpPath conf "kinded"
  dump fp r ( conf^.hkcDumpKcPretty
            , conf^.hkcDumpKcBin
            , conf^.hkcDumpKcJson
            , conf^.hkcDumpKcYaml
            )


dumpLc :: ( MonadIO m, HasHkcConfig c )
       => c -> Image -> m (Image Term Text (Pat) )
dumpLc conf r = do
  let fp = dumpPath conf "linear"
  dump fp r ( conf^.hkcDumpLcPretty
            , conf^.hkcDumpLcBin
            , conf^.hkcDumpLcJson
            , conf^.hkcDumpLcYaml
            )


-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

dumpPath :: HasHkcConfig c => c -> FilePath -> FilePath
dumpPath conf dumpDir =
  (conf^.hkcBuildDir) </> dumpDir </> takeBaseName (conf^.hkcOutFile)


dump :: ( MonadIO m
        , Binary a, PP.Pretty a, ToJSON a, FromJSON a )
     => FilePath -> a -> (Bool, Bool, Bool, Bool) -> m a
dump fp o (p, b, j, y) = do
  liftIO $ createDirectoryIfMissing True (takeDirectory fp)
  when p $ dumpPretty fp o
  when b $ dumpBinary fp o
  when j $ dumpJson fp o
  when y $ dumpYaml fp o
  return o
  

-----------------------------------------------------------------------
-- Various formats
-----------------------------------------------------------------------

dumpPretty :: (MonadIO m, PP.Pretty a)
           => FilePath -> a -> m ()
dumpPretty fp o = do
  let fp' = fp <.> "txt"
  liftIO $
    bracket (openFile fp' WriteMode) hClose
            (\h -> PP.hPutDoc h (PP.pretty o))
  return ()


dumpBinary :: (MonadIO m, Binary a)
           => FilePath -> a -> m ()
dumpBinary fp o = do
  let fp' = fp <.> "bin"
  liftIO $ encodeFile fp' o
  return ()


dumpJson :: (MonadIO m, ToJSON a)
         => FilePath -> a -> m ()
dumpJson fp o = do
  let fp' = fp <.> "json"
  liftIO $ LBS.writeFile fp' (JSN.encode o)
  return ()


dumpYaml :: (MonadIO m, ToJSON a)
         => FilePath -> a -> m ()
dumpYaml fp o = do
  let fp' = fp <.> "yaml"
  liftIO $ Y.encodeFile fp' o
  return ()
