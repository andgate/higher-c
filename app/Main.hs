module Main where

import Control.Applicative
import Data.Monoid
import Options.Applicative
import Language.Hawk.Compile.Options


main :: IO ()
main = do
    opts <- execParser optsParser
    -- Run compiler with given options
    return ()
  where
    optsParser =
        info
            (helper <*> versionOption <*> programOptions)
            (fullDesc <> progDesc "hkc" <>
             header
                 "hkc - a compiler for the Hawk programming language")
    versionOption = infoOption "0.0" (long "version" <> help "Show version")
    programOptions =
        Opts  <$> hsubparser (buildCommand <> cleanCommand)

              <*> option auto (long "verbosity" <> short 'v' <> value 1 <> metavar "LEVEL" <> help "Compiler message verbosity")
              <*> switch (long "f-info-filefound" <> help "Enable file found messages")
              <*> switch (long "f-info-dirfound" <> help "Enable directory found messages")
              <*> switch (long "f-info-freshmodules" <> help "Enable fresh module found messages")
              <*> switch (long "f-info-filefound" <> help "Enable file found messages")

              <*> option auto (long "warnings" <> short 'w' <> value 1 <> metavar "LEVEL" <> help "Compiler warning verbosity")
              <*> switch (long "f-warn-ignorefile" <> help "Enable file ignored warnings")
              <*> switch (long "f-warn-ignoredirectroy" <> help "Enable directory ignored warnings")
              <*> switch (long "f-warn-ignoresymlink" <> help "Enable symlink ignored warnings")
        
    buildCommand =
        command
            "build"
            (info buildOptions (progDesc "Build source files"))

    buildOptions =
        Build <$>
        strArgument (metavar "PATH" <> help "Path of package to build")

    cleanCommand =
        command
            "clean"
            (info (pure Clean) (progDesc "Clean all packages"))