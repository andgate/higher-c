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
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Tree
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Hawk.Compile.Monad
import System.FilePath ( takeExtension, takeBaseName, splitDirectories )

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Map                         as Map
import qualified Data.Streaming.Filesystem        as F
import qualified Data.Text.Lazy                   as T
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
  pkgId <- runSqlite "hk.db" $ do
    runMigration Db.migrateAll
    Db.insertPackage pkg

  -- Pipes
  runConduitRes
    $ sourceDirectoryDeep True (T.unpack d)
    .| filterC isValidModule
    .| mapC fpToMp
    .| buffer 5
    .| mapC toForest
    -- .| handleModule pkgId
    .| iterMC (\o -> lift $ print o)
    .| sinkNull


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
                Nothing -> return () 



toForest :: (Ord a) => [[a]] -> Forest a
toForest r = unfoldForest (\(a, rs) -> (a, levelEntries rs))
                          (levelEntries r)
  where
    levelMap :: (Ord a) => [[a]] -> Map a [[a]]
    levelMap aa = Map.fromListWith (++) [ (a, [as]) | (a:as) <- aa ]

    levelEntries :: (Ord a) => [[a]] -> [(a, [[a]])]
    levelEntries = Map.toList . levelMap


fromForest :: Forest a -> [[a]]
fromForest [] = [[]]
fromForest f  = concat [ map (a:) (fromForest subf) | Node a subf <- f ]

{-
handleModule :: MonadIO m => Db.PackageId -> Conduit (FilePath, F.FileType) m (FilePath, Db.ModuleId)
handleModule pkgId = awaitForever go
  where
    go :: MonadIO m => (FilePath, F.FileType) -> Conduit (FilePath, F.FileType) m (FilePath, Db.ModuleId)
    go (fp, ft) = do
      let mp = fpToMp fp
      mid <- liftIO $ cacheModule mp
      case ft of
          F.FTFile -> yield (fp, mid)
          F.FTFileSym -> yield (fp, mid)
          F.FTDirectory -> handleModule pkgId
          F.FTDirectorySym -> handleModule pkgId
          F.FTOther -> return ()

    cacheModule :: [T.Text] -> IO Db.ModuleId
    cacheModule mp =
      runSqlite "hk.db" $ do
        runMigration Db.migrateAll
        Db.insertModule pkgId mp
-}

fpToMp :: FilePath -> [T.Text]
fpToMp fp = mp
  where (_:mp) = map (T.pack . takeBaseName) (splitDirectories fp)


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