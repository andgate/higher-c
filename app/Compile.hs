module Compile where

import BuildInfo

import Language.Hawk.Parse.Lexer
import Language.Hawk.Parse.Parser

import System.IO

compile :: BuildInfo -> IO ()
compile info = do
  let p = srcPath info
  s <- readFile p
  let result = parse p s
  print result
