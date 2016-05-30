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

import qualified Language.Hawk.Parse.Helpers as Parser
import qualified Language.Hawk.Parse.Type as P
import qualified Language.Hawk.Parse.Variable as P
import qualified Language.Hawk.Parse.Function as P

import Text.Trifecta.Result

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
                
                let str = ":: Foo i32 -> i32 -> (i32, f64 -> Bool) -> void"
                    res = Parser.parseString P.typesig "(test)" str
                
                putStr "\nFile parsed:\n"
                print res
                
                
            it "Simple Variable Binding" $ do
                
                let str = "let foo :: f64 = add x 13 :: f32"
                    res = Parser.parseString P.var "(test)" str
                
                putStr "\nFile parsed:\n"
                print res
                
            it "Simple Function" $ do
                
                let str = "fn foo :: f64 -> f64 | x = x"
                    res = Parser.parseString P.function "(test)" str
                
                putStr "\nFile parsed:\n"
                print res

main :: IO ()
main = hspec spec