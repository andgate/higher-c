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

import qualified Language.Hawk.Parse.Binding as P
import qualified Language.Hawk.Parse.Type as P
import qualified Language.Hawk.Parse.Variable as P
import qualified Language.Hawk.Parse.Function as P
import qualified Language.Hawk.Parse.Module as P

spec :: Spec
spec = do
  describe "Parsing Examples" $ do
  
    context "Bindings Parsing" $ do
      it "Mutable Binding" $ do
          let str = ""
          P.mutable # str
        
      it "Immutable Binding" $ do
          let str = "!"
          P.immutable # str
      it "Immutable Binding (fails)" $ do
          let str = ""
          (P.immutable # str) `shouldThrow` anyErrorCall    
      it "Mutability: Mutable Binding" $ do
          let str = ""
          P.mutability # str
      it "Mutability: Immutable Binding" $ do
          let str = "!"
          P.mutability # str
          
      it "Mutable Binding By Value" $ do
          let str = ""
          P.byVal # str
      it "Mutable Binding By Reference" $ do
          let str = "&"
          P.byRef # str
      it "Evaluation: By Value" $ do
          let str = ""
          P.mutability # str
      it "Mutability: Immutable Binding" $ do
          let str = "!"
          P.mutability # str
          
          
    context "Type Parsing" $ do
      
      it "Simple Type" $ do
          
          let str = ":: (Foo F32 -> F32 -> (I32, F64 -> Bool) -> ())"
          P.typesig # str
          
    
    context "Variable Parsing" $ do
    
      it "Mutable Variable Binding with type" $ do
          
          let str = "sum :: I32 ^= 13"
          P.var # str
          
      it "Mutable Variable Binding without type" $ do
          
          let str = "sum ^= add 13 13"
          P.var # str
      
      it "Constant Variable Binding" $ do
          
          let str = "!sum :: I32 ^= add 13 13"
          P.var # str
          
      it "Mutable Reference Variable Binding" $ do
          
          let str = "&sum :: I32 ^= add 13 13"
          P.var # str    
          
      it "Constant Reference Variable Binding" $ do
          
          let str = "&!sum :: I32 ^= add 13 13"
          P.var # str
      
    context "Function Parsing" $ do
           
      it "Simple Function" $ do
          
          let str = "id x :: F64 -> F64 := return x"
          P.function # str
          
          
      it "Add and Double Function" $ do
          
          let str = "doubleSum x y :: I32 -> I32 -> I32 :=\n  sum :: I32 ^= add_i32 x y\n  sum = mul_i32 sum 2\n  return sum"
          P.function # str
      
      it "Test Function" $ do
          
          let str = "main foo :: IO () :=\n\n  car_a ^= Car 12 124\n  !car_b ^= Car 19 103\n  // Drive some cars\n  drive car_a\n  drive car_b"
          P.function # str
          
    context "Module Parsing" $ do
    
      it "example/grammar.hk" $ do
          r <- Parser.parseFromFile P.moduleInfo "example/main.hk"
          print $ show r

main :: IO ()
main = hspec spec