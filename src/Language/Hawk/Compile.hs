{-# LANGUAGE RankNTypes #-}
module Language.Hawk.Compile ( compile
                             , CompilerState (..)
                             , Package (..)
                             , defState
                             )
                             where

import Conduit
import Control.Monad ( forM_ )
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Maybe (catMaybes)
import Data.MonoTraversable (MonoFoldable, Element)
import Data.Text (Text)
import Data.Vector (Vector)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Hawk.Compile.Monad
import Language.Hawk.Compile.Source
import Language.Hawk.Parse.Document
import Language.Hawk.Parse.Lexer (lexer, tokenize)
import Language.Hawk.Parse.Lexer.Token (Token)
import Language.Hawk.Report.Result
import Language.Hawk.Report.Report (putReports)

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified Language.Hawk.Metadata           as Db
import qualified Language.Hawk.Metadata.Schema    as Db
import qualified Language.Hawk.Parse              as P
import qualified Language.Hawk.Report.Error       as Err
import qualified Language.Hawk.Report.Info        as Info
import qualified Language.Hawk.Report.Priority    as Pr
import qualified Language.Hawk.Report.Warning     as Warn
import qualified Language.Hawk.Syntax.Item        as I
import qualified Language.Hawk.Syntax.Expression  as E
import qualified Language.Hawk.Syntax.Module      as M



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

-- | Parse the modules in the given packages and store them accordingly
loadPackages :: Compiler ()
loadPackages = do
    s <- St.get
    -- Insert all the given packages
    mapM_ (liftIO . loadPackage) $ cPkgs s

loadPackage :: Package -> IO ()
loadPackage pkg@(Package n d) = do
  pid <- runSqlite "hk.db" $ do
    runMigration Db.migrateAll
    Db.staleAll
    Db.insertPackage pkg

  -- Pipes
  runConduitRes
    $ scanHawkSource (T.unpack d)
      .| conduitVector 1000
      .| mapC (\rs -> V.sequence rs)
      .| reportResultC
      .| sinkNull

{-    .| fetchDoc
    .| iterMC (\o -> lift $ print o)
    .| lexer
    .| iterMC (\o -> lift $ print o)
    .| P.itemParser
    .| iterMC (\o -> lift $ print o)
    .| takeC 7
    .| sinkNull
  -}

fetchDoc :: MonadResource m => Conduit InfoDoc m TextDoc
fetchDoc = awaitForever go
  where
    go (Doc mid fp _) =
      sourceFile fp .| decodeUtf8C .| mapC (Doc mid fp)


reportResultC :: MonadIO m => Conduit (Result a) m a
reportResultC = awaitForever go
  where
    go r = do
      liftIO $ putReports (resultReports Pr.None r)
      case getAnswer r of
        Nothing -> return ()
        Just v -> yield v


{-
cacheMods :: Db.PackageId -> Vector FilePath -> IO [InfoDoc]
cacheMods pid fps =
  runSqlite "hk.db" $ do
    runMigration Db.migrateAll
    Db.insertModules pid (V.toList fps)
-}


-- | Loads values n from the stream into memory.
--   Useful for forcing disk reads.
loadC :: (MonadBase base m, PrimMonad base) => Int -> Conduit a m a
loadC n =
  conduitVector n .| (yieldManyForever :: Monad m => Conduit (Vector a) m a)


-- | Releases values one at a time from a list
yieldManyForever :: (Monad m, MonoFoldable mono) => Conduit mono m (Element mono)
yieldManyForever = awaitForever yieldMany