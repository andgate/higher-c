module Main where

import HKC.Compile
import HKC.Build

import Options.Applicative

main :: IO ()
main = execParser opts >>= compile
  where
    parser = Build <$> argument str (metavar "FILE")
    opts = info parser mempty
