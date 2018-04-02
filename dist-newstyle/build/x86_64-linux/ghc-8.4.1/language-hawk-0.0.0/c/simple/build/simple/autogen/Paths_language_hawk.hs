{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_language_hawk (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/gabe/.cabal/bin"
libdir     = "/home/gabe/.cabal/lib/x86_64-linux-ghc-8.4.1/language-hawk-0.0.0-inplace-simple"
dynlibdir  = "/home/gabe/.cabal/lib/x86_64-linux-ghc-8.4.1"
datadir    = "/home/gabe/.cabal/share/x86_64-linux-ghc-8.4.1/language-hawk-0.0.0"
libexecdir = "/home/gabe/.cabal/libexec/x86_64-linux-ghc-8.4.1/language-hawk-0.0.0"
sysconfdir = "/home/gabe/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "language_hawk_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "language_hawk_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "language_hawk_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "language_hawk_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "language_hawk_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "language_hawk_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
