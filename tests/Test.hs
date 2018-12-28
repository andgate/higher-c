module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, findByExtension)

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, replaceExtension)
import System.Process (spawnCommand, waitForProcess)

import System.Exit (exitWith, ExitCode(..), die)


main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  srcPaths <- findByExtension [".hc"] "tests/single"
  createDirectoryIfMissing True "tests/out/single"
  createDirectoryIfMissing True "tests/bin/single"
  return $ testGroup "Higher-C Golden Tests"
    [ goldenVsFile
        ("Golden Test " ++ takeBaseName srcPath)
        goldPath
        outPath
        (runCompiler srcPath outPath)
    | srcPath <- srcPaths
    , let outPath = "tests/out/single/" <> takeBaseName srcPath <> ".out"
          goldPath = replaceExtension srcPath ".gold"
    ]


runCompiler :: FilePath -> FilePath -> IO ()
runCompiler srcPath outPath = do
  let exePath = "tests/bin/single/" <> takeBaseName srcPath <> ".exe"
  h1 <- spawnCommand $ "cabal new-run hcc -- " ++ srcPath ++ " -o " ++ exePath
  exit_code <- waitForProcess h1

  case exit_code of
    ExitSuccess -> return ()
    ExitFailure _ -> exitWith exit_code


  h2 <- spawnCommand $ exePath ++ " >> " ++ outPath
  exitWith =<< waitForProcess h2