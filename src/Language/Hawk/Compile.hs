{-# LANGUAGE RankNTypes
           , OverloadedStrings
           , FlexibleContexts
           , AllowAmbiguousTypes
  #-}
module Language.Hawk.Compile where

import Conduit
import Control.Lens
import Control.Monad (void)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Chronicle
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log
import Control.Monad.Reader
import qualified Control.Monad.State as St
import Control.Monad.Trans.Resource (MonadResource)
import Data.Foldable (forM_)
import Data.Maybe (catMaybes)
import Data.MonoTraversable (MonoFoldable, Element)
import Data.Text (Text)
import Data.Vector (Vector)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Hawk.Compile.Config
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Language.Hawk.Compile.Monad
import Language.Hawk.Compile.Source
import Language.Hawk.Compile.State
import Language.Hawk.Compile.Options
import Language.Hawk.Compile.Package
import Language.Hawk.Parse.Document
import Language.Hawk.Parse.Lexer (lexer, tokenize)
import Language.Hawk.Parse.Lexer.Token (Token)


import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified Language.Hawk.Cache.Item         as Db
import qualified Language.Hawk.Cache.Janitor      as Db
import qualified Language.Hawk.Cache.Module       as Db
import qualified Language.Hawk.Cache.Model        as Db
import qualified Language.Hawk.Cache.Package      as Db
import qualified Language.Hawk.Cache.Types        as Db
import qualified Language.Hawk.Parse              as P
import qualified Language.Hawk.Report.Error       as Err
import qualified Language.Hawk.Report.Info        as Info
import qualified Language.Hawk.Report.Warning     as Warn


hkc :: Hkc ()
hkc = compile

compile
  :: ( MonadReader c m
     , HasHkcConfig c, HasPackage c
     , St.MonadState s m
     , HasHkcState s, HasParseState s, HasNameCheckState s, HasTypeCheckState s
     , MonadLog (WithSeverity msg) m, AsLoaderMessage msg
     , MonadChronicle [e] m
     , AsHkcError e, AsLoaderError e, AsParseError e, AsNameCheckError e, AsTypeCheckError e
     , MonadIO m, MonadBase IO m, MonadBaseControl IO m, MonadCatch m)
  => m ()
compile = do
  loadModules
  --parseItems
  --namecheck
  --typecheck
  --optimize
  --codegen
  return ()

-- | Parse the modules in the given packages and store them accordingly
loadModules
  :: ( St.MonadState s m
     , HasHkcState s
     , MonadReader c m , HasHkcConfig c, HasPackage c
     , MonadLog (WithSeverity msg) m, AsLoaderMessage msg
     , MonadChronicle [e] m, AsLoaderError e
     , MonadIO m, MonadBase IO m, MonadBaseControl IO m, MonadCatch m)
  => m ()
loadModules = do
    runSqlite "hk.db" $ do
      runMigration Db.migrateAll
      Db.insertPackage

    condemn $
      runConduitRes $
        scanHawkSource
          -- .| conduitVector 100
          -- .| mapM_C (storeModules)
          .| sinkNull


storeModules
    ::  ( St.MonadState s m
        , HasHkcState s
        , MonadReader c m , HasHkcConfig c, HasPackage c
        , MonadLog (WithSeverity msg) m, AsLoaderMessage msg
        , MonadChronicle [e] m, AsLoaderError e
        , MonadIO m, MonadBase IO m, MonadBaseControl IO m, MonadResource m, MonadCatch m)
    => Vector HawkSource -> m ()
storeModules srcs =
  runSqlite "hk.db" $ do
    runMigration Db.migrateAll
    mapM_ Db.insertSource srcs

{-
loadFiles :: Opts -> Package -> IO ()
loadFiles o pkg@(Package n d) = do
  pid <- runSqlite "hk.db" $ do
    runMigration Db.migrateAll
    Db.insertPackage pkg

  runConduitRes
    $ scanHawkSource (T.unpack d)
      
      .| listConduit 100
      .| mapC Prelude.sequence
      .| reportResultC o
      .| mapC catMaybes

      .| mapM_C (liftIO . cacheMods pid)
      .| sinkNull
  
  runSqlite "hk.db" $ do
    runMigration Db.migrateAll
    
    Db.removeStale

    runConduitRes $
      selectSource 
          [ Db.ModuleFilePkg ==. pid
          , Db.ModuleFileCacheStatus ==. Db.Fresh
          , Db.ModuleFileIsBuilt ==. False
          ] []
        
        .| loadC 10
        .| mapC (handleModuleFileEntity pid)
        
        .| fetchDoc
        .| loadC 10
        .| lexer
--        .| iterMC (\(Doc _ _ toks) -> liftIO . putDoc . pretty $ toks)      
        
        .| P.itemParser
        .| reportResultC o
        
        .| mapM_C 
              (\(Doc pid mid mfid _ i)
                -> void $ lift $ Db.cache (pid, mid, mfid, i))
        
        .| reportResultC o
        .| sinkNull

-}

{-
handleModuleFileEntity :: Db.PackageId -> Entity Db.ModuleFile -> InfoDoc
handleModuleFileEntity pid (Entity mfid mf) =
  let mid = Db.moduleFileAssoc mf
      fp = T.unpack $ Db.moduleFilePath mf
  in Doc pid mid mfid fp ()


fetchDoc :: MonadResource m => Conduit InfoDoc m TextDoc
fetchDoc = awaitForever go
  where
    go d =
      sourceFile (d^.docPath) .| decodeUtf8C .| mapC (\t -> const t <$> d)


reportResultC :: MonadIO m => Opts -> Conduit (Result a) m a
reportResultC o = awaitForever go
  where
    go r = do
      liftIO $ putReports (o, r)
      forM_ (getAnswer r) yield

-}

-- | Loads values n from the stream into memory.
--   Useful for forcing disk reads.
loadC :: (MonadBase base m, PrimMonad base) => Int -> Conduit a m a
loadC n =
  conduitVector n .| (yieldManyForever :: Monad m => Conduit (Vector a) m a)


-- | Releases values one at a time from a list
yieldManyForever :: (Monad m, MonoFoldable mono) => Conduit mono m (Element mono)
yieldManyForever = awaitForever yieldMany

listConduit :: Monad m => Int -> Conduit a m [a]
listConduit n = go [] 0
  where 
    go xs m
        | n == m = do
            yield $ reverse xs
            listConduit n
        | otherwise = do
            mayx <- await 
            case mayx of
                Just x -> go (x:xs) (m+1)
                Nothing -> yield $ reverse xs