{-# LANGUAGE BangPatterns #-}
module Hawk.GrammarSpec where

import SpecHelper

import qualified Data.Map as Map


-- ASTs must be imported qualifed, and thus need to be imported here
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Compile as Compile
import qualified Language.Hawk.Report.Result as Result

import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)

spec :: Spec
spec = do
    describe "Parsing Examples" $ do
    
        context "AST Generation" $ do
            it "example/grammar.hk" $ do
                let res = Compile.compile Package.dummyName "example/grammar.hk"
                
                putStr "\nFile parsed:\n"
                --print src_ast
                
                --True `shouldBe` False

main :: IO ()
main = hspec spec