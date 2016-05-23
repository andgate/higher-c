module Main where

import Compile
import BuildInfo

main :: IO ()
main = return ()
  where
    build_conf = BuildInfo "example/main.hk"
