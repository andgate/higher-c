{-# LANGUAGE RankNTypes
           , OverloadedStrings
  #-}
module Language.Hawk.Compile ( compile
                             , CompilerState (..)
                             , Package (..)
                             , defState
                             )
                             where

import Conduit
import Control.Lens
import Control.Monad (void)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Foldable (forM_)
import Data.Maybe (catMaybes)
import Data.MonoTraversable (MonoFoldable, Element)
import Data.Text (Text)
import Data.Vector (Vector)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Hawk.Compile.Monad
import Language.Hawk.Compile.Source
import Language.Hawk.Compile.Options
import Language.Hawk.Parse.Document
import Language.Hawk.Parse.Lexer (lexer, tokenize)
import Language.Hawk.Parse.Lexer.Token (Token)
import Language.Hawk.Report.Result
import Language.Hawk.Report.Report (putReports, toReports)
import Text.PrettyPrint.ANSI.Leijen (pretty, putDoc)


import qualified Control.Monad.Trans.State.Strict as St
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



compile
  :: CompilerState
  -> IO ()
compile s =
  runCompiler s $
    do
      loadPackages
      --typecheck
      --optimize
      --codegen
      return ()

-- | Parse the modules in the given packages and store them accordingly
loadPackages :: Compiler ()
loadPackages = do
    s <- St.get
    let o = cOpts s
        pkgs = cPkgs s
    -- Insert all the modules for the given package
    liftIO $ mapM_ (loadFiles o) pkgs

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

cacheMods :: Db.PackageId -> [HawkSource] -> IO ()
cacheMods pid fps =
  runSqlite "hk.db" $ do
    runMigration Db.migrateAll
    mapM_ (Db.insertSource pid) fps


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