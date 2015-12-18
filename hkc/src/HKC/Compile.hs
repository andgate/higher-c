module HKC.Compile where

import HKC.Build

import HKC.Tokens
import HKC.Grammar

import System.IO

compile :: Build -> IO ()
compile build = do
  let p = srcPath build
  s <- readFile p
  let result = parse p s
  print result
