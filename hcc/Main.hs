{-# Language TemplateHaskell
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
           , LambdaCase
           , FlexibleContexts
  #-}
module Main where

{-
What input does the compiler take?

- Basic syntax
hcc <in..>
Passing just source files will
cause the compiler to check the files.
However, no ouput is produced.

- Simple output is produced with the '-o' flag.
hcc [options] file...

-- Ouputs are
.hci
.o
.exe
.lib
.dll
.dll.a
.so
.lib

- Executable binaries
hcc Main.hc main.exe

- Object files
hcc Moo.hc -o foo.o

- Windows libaries
hcc A.hc B.hc C.hc -o MyLib.dll

Note: This will also generate .lib and .dll.a files.

- Linux libaries
hcc src -o MyLib.so

- You can specify a source directory
hcc src -o program.exe

And hcc will search for higher-c source files
in that directory. Specify recursive search
with '-r'.

hcc -r src -o program.exe

-}

import Prelude hiding (lex)

import Paths_language_higher_c

import Control.Monad
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Version
import System.Console.GetOpt
import System.Environment

import Language.HigherC.Compile (readObject, build)
import Language.HigherC.Syntax.Concrete (Object)

import TestModule

import qualified Data.Text as T
import qualified Data.Text.IO as T


import Data.Tree (drawTree)
import Data.Graph (scc)

import qualified Data.Graph as G



data Flag
  = IncludePath FilePath
  | LibraryPath FilePath
  | OutputDir FilePath

data Options = Options
  { optShowVersion :: Bool 
  , optInput       :: [FilePath]
  , optOutput      :: [FilePath]
  , optLibDirs     :: [FilePath]
  } deriving Show

defaultOptions =
  Options
    { optShowVersion = False
    , optInput       = []
    , optOutput      = []
    , optLibDirs     = []
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v','?'] ["version"]
      (NoArg (\ opts -> opts { optShowVersion = True }))
      "show version number"

  , Option ['i']     []
      (ReqArg (\ arg opts -> opts { optInput = optInput opts ++ [arg] })
       "FILE")
       "input FILE"

  , Option ['o']     []
      (ReqArg (\ arg opts -> opts { optOutput = optInput opts ++ [arg] })
      "FILE")
      "output FILE"

  , Option ['L']     ["libdir"]
      (ReqArg (\ d opts -> opts { optLibDirs = optLibDirs opts ++ [d] })
      "DIR")
      "library directory"
  ]



compilerOpts :: [String] -> IO Options
compilerOpts argv =
  case getOpt Permute options argv of
    (o,fs,[]  ) -> do
      let opts = foldl (flip id) defaultOptions o   -- Explaination: https://stackoverflow.com/questions/32343586/getopt-usage-and-foldl-flip-id
      return (opts { optInput = optInput opts ++ fs })

    (_,_,errs) ->
      ioError (userError (concat errs ++ usageInfo header options))

  where
    header = "Usage: hcc [options] file..."

main :: IO ()
main = do
  argv <- getArgs
  opts <- compilerOpts argv
  print opts

  when (optShowVersion opts)
       (putStrLn $ "hcc version " ++ (showVersion version))

  writeTestModule

  objs <- mapM readObject (optInput opts)
  buildRes <- runExceptT $ build objs []

  -- Report errors
  case buildRes of
    Left err -> putDoc (pretty err)
    Right _  -> return ()

  return ()
