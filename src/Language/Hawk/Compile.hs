
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

import Language.Hawk.Syntax
import Language.Hawk.Load
import Language.Hawk.Load.Result (LdResult)
import Language.Hawk.Lex
import Language.Hawk.Lex.Result (LxResult)
import Language.Hawk.Lex.Token
import Language.Hawk.Parse
import Language.Hawk.Parse.Result (PsResult)
import Language.Hawk.NameCheck
import Language.Hawk.NameCheck.Result (NcResult)
import Language.Hawk.TypeCheck
import Language.Hawk.TypeCheck.Result (TcResult)
import Language.Hawk.KindsCheck
import Language.Hawk.KindsCheck.Result (KcResult)
import Language.Hawk.LinearCheck
import Language.Hawk.LinearCheck.Result (LcResult)

import Language.Hawk.Compile.Config
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Language.Hawk.Compile.Monad
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeBaseName)
import System.IO (hClose, openFile, IOMode(..))
import Text.PrettyPrint.Leijen.Text (pretty)


import qualified Data.Aeson            as JSN
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString       as BS
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Data.Vector           as V
import qualified Data.Map.Strict       as Map
import qualified Language.Hawk.Parse   as P
import qualified Language.Hawk.Parse.Result as PsR
import qualified Language.Hawk.TypeCheck as Tc
import qualified Language.Hawk.TypeCheck.Result as TcR
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
      >>= lexMany             >>= dumpLx conf
      >>= parseMany           >>= dumpPs conf
      >>= Nc.namecheck        >>= dumpNc conf
      >>= Tc.typecheckMany    >>= dumpTc conf  

  return ()



dumpPath :: HasHkcConfig c => c -> FilePath -> FilePath
dumpPath conf dumpDir =
  (conf^.hkcBuildDir) </> dumpDir </> takeBaseName (conf^.hkcOutFile)



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
       => c -> PsResult -> m PsResult
dumpPs conf r = do
  let fp = dumpPath conf "parse"
  dump fp r ( conf^.hkcDumpPsPretty
            , conf^.hkcDumpPsBin
            , conf^.hkcDumpPsJson
            , conf^.hkcDumpPsYaml
            )


dumpNc :: ( MonadIO m, HasHkcConfig c )
       => c -> NcResult -> m NcResult
dumpNc conf r = do
  let fp = dumpPath conf "named"
  dump fp r ( conf^.hkcDumpNcPretty
            , conf^.hkcDumpNcBin
            , conf^.hkcDumpNcJson
            , conf^.hkcDumpNcYaml
            )


dumpTc :: ( MonadIO m, HasHkcConfig c )
       => c -> TcResult -> m TcResult
dumpTc conf r = do
  let fp = dumpPath conf "typed"
  dump fp r ( conf^.hkcDumpTcPretty
            , conf^.hkcDumpTcBin
            , conf^.hkcDumpTcJson
            , conf^.hkcDumpTcYaml
            )


dumpKc :: ( MonadIO m, HasHkcConfig c )
       => c -> KcResult -> m KcResult
dumpKc conf r = do
  let fp = dumpPath conf "kinded"
  dump fp r ( conf^.hkcDumpKcPretty
            , conf^.hkcDumpKcBin
            , conf^.hkcDumpKcJson
            , conf^.hkcDumpKcYaml
            )


dumpLc :: ( MonadIO m, HasHkcConfig c )
       => c -> LcResult -> m LcResult
dumpLc conf r = do
  let fp = dumpPath conf "linear"
  dump fp r ( conf^.hkcDumpLcPretty
            , conf^.hkcDumpLcBin
            , conf^.hkcDumpLcJson
            , conf^.hkcDumpLcYaml
            )

    
dump :: ( MonadIO m
        , Binary a, PP.Pretty a, ToJSON a, FromJSON a )
     => FilePath -> a -> (Bool, Bool, Bool, Bool) -> m a
dump fp o (p, b, j, y) = do
  when p $ dumpPretty fp o
  when b $ dumpBinary fp o
  when j $ dumpJson fp o
  when y $ dumpYaml fp o
  return o


dumpPretty :: (MonadIO m, PP.Pretty a)
           => FilePath -> a -> m ()
dumpPretty fp o = do
  let fp' = fp <.> "txt"
  liftIO $ do
    createDirectoryIfMissing True fp 
    bracket (openFile fp' WriteMode) hClose
            (\h -> PP.hPutDoc h (PP.pretty o))
  return ()


dumpBinary :: (MonadIO m, Binary a)
           => FilePath -> a -> m ()
dumpBinary fp o = do
  let fp' = fp <.> "bin"
  liftIO $ do
    createDirectoryIfMissing True fp 
    encodeFile fp' o
  return ()


dumpJson :: (MonadIO m, ToJSON a)
         => FilePath -> a -> m ()
dumpJson fp o = do
  let fp' = fp <.> "json"
  liftIO $ do
    createDirectoryIfMissing True fp 
    LBS.writeFile fp' (JSN.encode o)
  return ()


dumpYaml :: (MonadIO m, ToJSON a)
         => FilePath -> a -> m ()
dumpYaml fp o = do
  let fp' = fp <.> "yaml"
  liftIO $ do
    createDirectoryIfMissing True fp 
    Y.encodeFile fp' o
  return ()


