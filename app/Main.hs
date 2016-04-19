module Main where

import Compile
import BuildInfo

main :: IO ()
main = compile build_conf
  where
    build_conf = BuildInfo "example/main.hk"
