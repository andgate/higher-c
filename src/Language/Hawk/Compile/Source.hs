{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Language.Hawk.Compile.Source where

import Conduit
import Control.Lens
import Control.Monad.Chronicle
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.Trans.Resource (MonadResource)
import Data.Char (isUpper, isAlphaNum)
import Data.List (intercalate, mapAccumL)
import Data.Time.Clock (UTCTime)
import Data.Streaming.Filesystem (getFileType, FileType(..))
import Language.Hawk.Compile.Error
import Language.Hawk.Compile.Message
import Language.Hawk.Compile.Package
import System.Directory (getModificationTime)
import System.FilePath ( takeExtension, takeBaseName, splitDirectories, makeRelative )

import qualified Language.Hawk.Report.Error       as Err

data HawkSource =
    HkSrc
    { srcPath :: FilePath
    , srcFileType :: FileType
    , srcTimestamp :: UTCTime
    } deriving Show

{- |
      Turns a filepath into a module path.

      >>> import Data.Time.Clock (getCurrentTime)
      >>> clk <- getCurrentTime
      >>> modulePath $ HkSrc "src/" "src/Foo/Bar/Baz.hk" FTFile clk
      "Foo.Bar.Baz"
-}
modulePath :: (MonadReader c m, HasPackage c)
           => HawkSource -> m String
modulePath src =
  intercalate "." <$> splitModulePath src


splitModulePath :: (MonadReader c m, HasPackage c)
                => HawkSource -> m [String]
splitModulePath src = do
  root <- view (package.pkgSrcDir)
  return $ map takeBaseName . splitDirectories $ makeRelative root (srcPath src)


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
      [c] -> isUpper c
      (c:cs) -> isUpper c && any isAlphaNum cs
    



scanHawkSource 
    :: ( MonadIO m, MonadBase IO m, MonadBaseControl IO m, MonadResource m
       , MonadReader c m, HasPackage c
       , MonadLog (WithSeverity msg) m, AsLoaderMessage msg
       , MonadChronicle [e] m, AsLoaderError e
       )
    => Producer m HawkSource
scanHawkSource = do
    root <- view $ package.pkgSrcDir
    start root
    
  where
    start ::  
      ( MonadIO m, MonadBase IO m, MonadBaseControl IO m, MonadResource m
      , MonadReader c m, HasPackage c
      , MonadLog (WithSeverity msg) m, AsLoaderMessage msg
      , MonadChronicle [e] m, AsLoaderError e
      ) => FilePath -> Producer m HawkSource
    start dir = sourceDirectory dir .| awaitForever go

    go :: ( MonadIO m, MonadBase IO m, MonadBaseControl IO m, MonadResource m
          , MonadReader c m, HasPackage c
          , MonadLog (WithSeverity msg) m, AsLoaderMessage msg
          , MonadChronicle [e] m, AsLoaderError e
          )
        => FilePath -> Producer m HawkSource
    go fp = do
        ft <- liftIO $ getFileType fp
        ts <- liftIO $ getModificationTime fp
        let src = HkSrc fp ft ts
        case ft of
            FTFile          ->    if not $ isHkFile fp then
                                    lift $ logMessage $ WithSeverity Warning $ review _WarnFileIgnored fp

                                  else if not $ isAcceptedModuleName fp then
                                    lift $ do
                                      logMessage $ WithSeverity Error	$ review (_LoaderErrMsg . _BadModuleName) fp
                                      disclose $ [review _BadModuleName fp]

                                  else do
                                    lift $ logMessage $ WithSeverity Informational	$ review _FileFound fp
                                    yield src
                                    

            FTFileSym       ->  lift $ logMessage $ WithSeverity Warning $ review _WarnSymLinkIgnored fp

            FTDirectory     ->  if not $ isAcceptedModuleName fp
                                  then lift $ do
                                    logMessage $ WithSeverity Error	$ review (_LoaderErrMsg . _BadModuleName) fp
                                    disclose $ [review _BadModuleName fp]

                                  else do
                                    lift $
                                      logMessage $ WithSeverity Informational	$ review _DirectoryFound fp
                                    yield src
                                    start fp


            FTDirectorySym  -> 
              lift $
                logMessage $ WithSeverity Warning	$ review _WarnSymLinkIgnored fp

            FTOther         ->  return ()
          