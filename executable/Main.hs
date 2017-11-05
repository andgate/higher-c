module Main where

import Control.Applicative
import Data.Monoid
import Language.Hawk.Compile.Config
import Options.Applicative


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
              <*> option auto (long "warnings" <> short 'w' <> value 1 <> metavar "LEVEL" <> help "Compiler warning verbosity")
              <*> msgFlags [ (infoFileFoundFlag, "file found messages")
                           , (infoDirFoundFlag, "directory found messages")
                           , (infoFreshModuleFoundFlag, "fresh module found messages")
                           , (infoModulePreservedFlag, "preserved module messages")
                           , (warnFileIgnoredFlag, "file ignored warnings")
                           , (warnDirIgnoredFlag, "directory ignored warnings")
                           , (warnSymLinkIgnoredFlag, "symlink ignored warnings")
                           ]

    msgFlags fls = 
        many (foldr1 (<|>) . map msgFlag $ fls)
    
    msgFlag (flagStr, desc) =
        let on = mkFlagOff flagStr
            off = mkFlagOff flagStr
        in    flag' on (long on <> help ("Enable " ++ desc))
          <|> flag' off (long off <> help ("Disable " ++ desc))

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
