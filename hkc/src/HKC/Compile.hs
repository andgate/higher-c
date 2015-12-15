module HKC.Compile where

import HKC.Build

import HKC.Tokens
import HKC.Grammar

import System.IO

compile :: Build -> IO ()
compile build = do
  let p = srcPath build
  s <- readFile p
  print p
  print $ s ++ "\n"

  let ts = scanTokens s
  putStrLn . show $ ts

  let ast = parseSrc ts
  putStrLn . show $ ast
