module Main where

import Hawk.Compile
import Hawk.Build

main :: IO ()
main = compile build_conf
  where
    build_conf = Build "main.hk"
