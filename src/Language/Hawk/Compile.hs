{-# LANGUAGE RankNTypes #-}
module Language.Hawk.Compile ( compile
                             , CompilerState (..)
                             , Package (..)
                             , defState
                             )
                             where

import Conduit
import Control.Monad ( forM_ )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Char (isUpper)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Hawk.Compile.Monad
import Language.Hawk.Parse.Lexer (lexer, tokenize)
import System.FilePath ( (</>), (<.>), takeExtension, takeBaseName, splitDirectories )

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Streaming.Filesystem        as F
import qualified Data.Text                        as T
import qualified Language.Hawk.Metadata           as Db
import qualified Language.Hawk.Metadata.Schema    as Db
import qualified Language.Hawk.Parse              as P
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
    Db.insertPackage pkg

  -- Pipes
  runConduitRes
    $ sourceDirectoryDeep True (T.unpack d)
    .| moduleLoader pid
    .| fileFetcher
    .| lexer
    .| iterMC (\o -> lift $ print o)
    .| sinkNull

  where
    moduleLoader pid =
      filterC isValidModule   -- Discards files that aren't module files
      .| buffer 100           -- Accumulates some module filepaths
      .| mapMC (\i -> liftIO $ cacheMods pid i)    -- Cache the modules on disk
      .| unbuffer           -- unbuffer modules, for handling files one at a time



fileFetcher :: MonadResource m => Conduit (FilePath, Db.ModuleId) m (T.Text, Db.ModuleId)
fileFetcher = awaitForever go
  where
    go (fp, mid) =
      sourceFile fp .| decodeUtf8C .| mapC (\t -> (t, mid))
    


cacheMods :: Db.PackageId -> [FilePath] -> IO [(FilePath, Db.ModuleId)]
cacheMods pid fps =
  runSqlite "hk.db" $ do
    runMigration Db.migrateAll
    Db.insertModules pid fps

-- | Captures n values from the stream and places into a list
buffer :: Monad m => Int -> Conduit a m [a]
buffer nlimit = go [] 0
    where
      go xs n
        | n == nlimit = do
            yield $ reverse xs
            buffer nlimit
        | otherwise = do
            mayx <- await 
            case mayx of
                Just x -> go (x:xs) (n+1)
                Nothing -> yield $ reverse xs


-- | Releases values one at a time from a list
unbuffer :: Monad m => Conduit [a] m a
unbuffer = go
    where
      go = do
        mayxs <- await
        case mayxs of
            Just xs -> release xs
            Nothing -> return ()

      release [] = unbuffer
      release (x:xs) = yield x >> release xs


---handleItems :: Package -> Conduit FilePath m FilePath

    
-- Would be nice if this warned against
-- some files, like lowercase .hk files.
-- Or maybe i should disregard case from the requirements.
isValidModule :: FilePath -> Bool
isValidModule fp = isHkSrc && isCap
  where
    (x:_) = takeBaseName fp
    ext = takeExtension fp
    isHkSrc = ext == ".hk"
    isCap = isUpper x


{-
recurseDirectory  :: MonadResource m
                  => FilePath -- ^ Root directory
                  -> Producer m (FilePath, F.FileType)
recurseDirectory = start
  where
    start :: MonadResource m => FilePath -> Producer m (FilePath, F.FileType)
    start dir = sourceDirectory dir .| awaitForever go

    go :: MonadResource m => FilePath -> Producer m (FilePath, F.FileType)
    go fp = do
        ft <- liftIO $ F.getFileType fp
        case ft of
            F.FTFile -> yield fp
            F.FTFileSym -> yield fp
            F.FTDirectory -> yield fp >> start fp
            F.FTDirectorySym -> yield fp >> start fp
            F.FTOther -> return ()

-}