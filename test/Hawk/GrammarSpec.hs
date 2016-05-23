{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hawk.GrammarSpec where

import SpecHelper

import qualified Data.Map as Map


-- ASTs must be imported qualifed, and thus need to be imported here
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Compile as Compile
import qualified Language.Hawk.Report.Result as Result

import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)

import qualified Language.Hawk.Parse.Type as TyP

import Text.Trifecta.Parser
import Text.Trifecta.Delta

spec :: Spec
spec = do
    describe "Parsing Examples" $ do
    
        context "AST Generation" $ do
            {-
            it "example/grammar.hk" $ do
                let res = Compile.compile Package.dummyName "example/grammar.hk"
                
                putStr "\nFile parsed:\n"
                --print src_ast
                
                --True `shouldBe` False
            -}
                
            it "Simple Type" $ do
                
                let type_str = ":: Foo i32 -> i32 -> (i32, i32)->void"
                    delta = (Directed "(test)" 0 0 0 0)
                    res = parseString TyP.typesig delta type_str
                
                putStr "\nFile parsed:\n"
                print res
                
                --True `shouldBe` False

main :: IO ()
main = hspec spec