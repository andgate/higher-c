{-# LANGUAGE RankNTypes #-}
module Language.Hawk.Compile.Source where

import Conduit
import Control.Monad.Trans.Resource (MonadResource)
import Data.Char (isUpper, isAlphaNum)
import Data.List (intercalate, mapAccumL)
import Data.Time.Clock (UTCTime)
import Data.Streaming.Filesystem (getFileType, FileType(..))
import Language.Hawk.Report.Result
import System.Directory (getModificationTime)
import System.FilePath ( (</>), (<.>), takeExtension, takeBaseName, splitDirectories, makeRelative )

import qualified Language.Hawk.Report.Error       as Err
import qualified Language.Hawk.Report.Info        as Info
import qualified Language.Hawk.Report.Warning     as Warn

data HawkSource =
    HkSrc
    { srcRoot :: FilePath
    , srcPath :: FilePath
    , srcFileType :: FileType
    , srcTimestamp :: UTCTime
    } deriving Show


modulePath :: HawkSource -> String
modulePath =
  intercalate "." . splitModulePath

splitModulePath :: HawkSource -> [String]
splitModulePath src = map takeBaseName . splitDirectories $ makeRelative root path
  where
    root = srcRoot src
    path = srcPath src


qualifyModulePath :: [String] -> [(String, String)]
qualifyModulePath ns = r
  where
    (_, r) = mapAccumL f [] ns
    f a b =
      let a' = a ++ [b]
      in (a', (b, intercalate "." a'))


moduleName :: HawkSource -> String
moduleName = takeBaseName . srcPath

isHkFile :: FilePath -> Bool
isHkFile fp = takeExtension fp == ".hk"

isAcceptedModuleName :: FilePath -> Bool
isAcceptedModuleName fp = 
  let
    n = takeBaseName fp
  in 
    case n of
      [] -> False
      (c:[]) -> isUpper c
      (c:cs) -> isUpper c && any isAlphaNum cs
    



scanHawkSource  :: MonadResource m
                  => FilePath -- ^ Root directory
                  -> Producer m (Result (Maybe HawkSource))
scanHawkSource root = start root
  where
    start :: MonadResource m => FilePath -> Producer m (Result (Maybe HawkSource))
    start dir = sourceDirectory dir .| awaitForever go

    go :: MonadResource m => FilePath -> Producer m (Result (Maybe HawkSource))
    go fp = do
        ft <- liftIO $ getFileType fp
        ts <- liftIO $ getModificationTime fp
        let src = HkSrc root fp ft ts
        case ft of
            FTFile          ->  yield $
                                  if not $ isHkFile fp then do
                                    warn $ Warn.FileIgnored fp
                                    return Nothing

                                  else if not $ isAcceptedModuleName fp then do
                                    throw $ Err.BadModuleName fp
                                    return Nothing

                                  else do
                                    info $ Info.FileFound fp
                                    return (Just src)
                                    

            FTFileSym       ->  yield $ do
                                  warn $ Warn.SymLinkIgnored fp
                                  return Nothing


            FTDirectory     ->  if not $ isAcceptedModuleName fp then
                                    yield $ do
                                      throw $ Err.BadModuleName fp
                                      return Nothing

                                else do
                                  yield $ do
                                    info $ Info.DirectoryFound fp
                                    return (Just src)
                                  start fp


            FTDirectorySym  ->  yield $ do
                                  warn $ Warn.SymLinkIgnored fp
                                  return Nothing

            FTOther         ->  return ()
          