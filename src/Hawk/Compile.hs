module Hawk.Compile where

import Hawk.Build

import Hawk.Tokens
import Hawk.Grammar

import System.IO

compile :: Build -> IO ()
compile build = do
  let p = srcPath build
  s <- readFile p
  let result = parse p s
  print result
