module Main where

import Control.Applicative
import Data.Monoid
import Language.Hawk.Compile.Config
import Language.Hawk.Compile (hkc)
import Options.Applicative


main :: IO ()
main = do
    hkc =<< execParser optsParser 
  where
    optsParser =
        info
            (helper <*> versionOption <*> programOptions)
            (fullDesc <> progDesc "hkc" <>
             header
                 "hkc - a compiler for the Hawk programming language")
    versionOption = infoOption "0.1.0" (long "version" <> help "Show version")
    programOptions =
        HkcConfig
          -- Source files
          <$> some (argument str (metavar "FILES..."))
          -- Output file
          <*> strOption ( long "output" <> short 'o' <> metavar "FILENAME")
          -- Output type
          <*> (flag' HkcOutBin (long "bin") <|> flag' HkcOutLib (long "lib") <|> pure HkcOutBin) 
          -- Build directory
          <*> strOption ( long "build-dir" <> metavar "DIRECTORYNAME")
          -- Interactive
          <*> switch ( long "interactive" <> short 'i' <> help "Run hkc in interactive-mode." )

          -- Dump Lexical Output
          <*> switch ( long "dump-lx-pretty" )
          <*> switch ( long "dump-lx-binary" )
          <*> switch ( long "dump-lx-json" )
          <*> switch ( long "dump-lx-yaml" )

          -- Dump Parser Output
          <*> switch ( long "dump-ps-pretty" )
          <*> switch ( long "dump-ps-binary" )
          <*> switch ( long "dump-ps-json" )
          <*> switch ( long "dump-ps-yaml" )
 
          -- Dump Name Checked Ast 
          <*> switch ( long "dump-nc-pretty" )
          <*> switch ( long "dump-nc-binary" )
          <*> switch ( long "dump-nc-json" )
          <*> switch ( long "dump-nc-yaml" )

          -- Dump Type Checked Ast
          <*> switch ( long "dump-tc-pretty" )
          <*> switch ( long "dump-tc-binary" )
          <*> switch ( long "dump-tc-json" )
          <*> switch ( long "dump-tc-yaml" )

          -- Dump Kinds Checked Ast 
          <*> switch ( long "dump-kc-pretty" )
          <*> switch ( long "dump-kc-binary" )
          <*> switch ( long "dump-kc-json" )
          <*> switch ( long "dump-kc-yaml" )

          -- Dump Linearity Checked Ast
          <*> switch ( long "dump-lc-pretty" )
          <*> switch ( long "dump-lc-binary" )
          <*> switch ( long "dump-lc-json" )
          <*> switch ( long "dump-lc-yaml" )

          -- Dump Generated LLVM
          <*> switch ( long "dump-llvm" )
