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
          -- Dump Lexical Output 
          <*> (flag' (Just AstPretty) (long "dump-tokens" <> help "Dump tokens produced by hkc's lexer.") <|> pure Nothing)
          -- Dump Parser Output 
          <*> (flag' (Just AstPretty) (long "dump-tokens" <> help "Dump tokens produced by hkc's lexer.") <|> pure Nothing)
          -- Dump Name Checked Ast 
          <*> (flag' (Just AstPretty) (long "dump-tokens" <> help "Dump tokens produced by hkc's lexer.") <|> pure Nothing)
          -- Dump Type Checked Ast
          <*> (flag' (Just AstPretty) (long "dump-tokens" <> help "Dump tokens produced by hkc's lexer.") <|> pure Nothing)
          -- Dump Kinds Checked Ast 
          <*> (flag' (Just AstPretty) (long "dump-tokens" <> help "Dump tokens produced by hkc's lexer.") <|> pure Nothing)
          -- Dump Generated LLVM
          <*> switch (long "dump-llvm")
          -- Interactive Mode
          <*> switch ( long "interactive" <> short 'i' <> help "Run hkc in interactive-mode." )
          
