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
import Data.Char (isUpper)
import Data.MonoTraversable (MonoFoldable, Element)
import Data.Text (Text)
import Data.Vector (Vector)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Hawk.Compile.Monad
import Language.Hawk.Parse.Document
import Language.Hawk.Parse.Lexer (lexer, tokenize)
import Language.Hawk.Parse.Lexer.Token (Token)

import System.Directory (getModificationTime)
import System.FilePath ( (</>), (<.>), takeExtension, takeBaseName, splitDirectories )

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Streaming.Filesystem        as F
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
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
    .| iterMC (\o -> lift $ print o)
    .| fetchDoc
    .| iterMC (\o -> lift $ print o)
    .| lexer
    .| iterMC (\o -> lift $ print o)
    .| P.itemParser
    .| iterMC (\o -> lift $ print o)
    .| takeC 7
    .| sinkNull

  where
    moduleLoader pid =
      filterC isValidModule        -- Discards files that aren't module files
      .| conduitVector 1000        -- Accumulates filepaths from disk to ram
      .| mapMC (\i -> liftIO $ cacheMods pid i)    -- Cache the modules on disk
      .| yieldManyForever   -- unbuffer modules, for handling files one at a time

type ScanResult = Result FilePath


fetchDoc :: MonadResource m => Conduit InfoDoc m TextDoc
fetchDoc = awaitForever go
  where
    go (Doc mid fp _) =
      sourceFile fp .| decodeUtf8C .| mapC (Doc mid fp)



cacheMods :: Db.PackageId -> Vector FilePath -> IO [InfoDoc]
cacheMods pid fps =
  runSqlite "hk.db" $ do
    runMigration Db.migrateAll
    Db.insertModules pid (V.toList fps)


-- | Loads values n from the stream into memory.
--   Useful for forcing disk reads.
loadC :: (MonadBase base m, PrimMonad base) => Int -> Conduit a m a
loadC n =
  conduitVector n .| (yieldManyForever :: Monad m => Conduit (Vector a) m a)


-- | Releases values one at a time from a list
yieldManyForever :: (Monad m, MonoFoldable mono) => Conduit mono m (Element mono)
yieldManyForever = awaitForever yieldMany



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


data HawkSource =
    HkSrc
    { srcPath :: FilePath
    , 
    } 

hawkSource :: Text -> MonadResource m => Source m (Result FilePath)
hawkSource d =  sourceDirectoryDeep True (T.unpack d) .| awaitForever go
    where
      go fp =

        where
          ext = takeExtension fp
          isHkSrc = ext == ".hk"
          isValidModulePath = all (isUpper . head) . tail . splitDirectories $ fp

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
        ts <- liftIO $ getModificationTime fp
        case ft of
            F.FTFile -> yield fp
            F.FTFileSym -> yield fp
            F.FTDirectory -> yield fp >> start fp
            F.FTDirectorySym -> yield fp >> start fp
            F.FTOther -> return ()