{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hawk.GrammarSpec where

import SpecHelper

import qualified Data.ByteString as BS

import qualified Data.Map as Map

import qualified Data.Yaml as YAML
import qualified Data.Yaml.Pretty as YAML

-- ASTs must be imported qualifed, and thus need to be imported here
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Compile as Compile
import qualified Language.Hawk.Report.Result as Result

import  Language.Hawk.Parse.Helpers ((#))
import qualified Language.Hawk.Parse.Helpers as Parser
import qualified Language.Hawk.Parse.Type as P
import qualified Language.Hawk.Parse.Variable as P
import qualified Language.Hawk.Parse.Function as P

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
          P.typesig # str
          
          
      it "Simple Variable Binding" $ do
          
          let str = "let foo :: f64 = add x 13 :: f32"
          P.var # str
          
          
      it "Simple Function" $ do
          
          let str = "fn foo :: f64 -> f64 | x = x"
          P.function # str
          
          
      it "Add and Double Function" $ do
          
          let str = "fn doubleSum :: i32 -> i32 -> i32 | x y :\n  let sum :: i32 = add_i32 x y\n  let z :: i32 = mul_i32 sum 2\n  return z"
          P.function # str

main :: IO ()
main = hspec spec