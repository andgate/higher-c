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
import qualified Language.Hawk.Parse.Module as P

spec :: Spec
spec = do
  describe "Parsing Examples" $ do
  
    context "AST Generation" $ do
    
      it "example/grammar.hk" $ do
          r <- Parser.parseFromFile P.moduleUnits "example/main.hk"
          print $ show r
      
          
      it "Simple Type" $ do
          
          let str = ":: (Foo F32 -> F32 -> (I32, F64 -> Bool) -> ())"
          P.typesig # str
          
          
      it "Simple Variable Binding" $ do
          
          let str = "sum :: I32 $= add 13 13"
          P.var # str
          
          
      it "Simple Function" $ do
          
          let str = "id x :: F64 -> F64 := x"
          P.function # str
          
          
      it "Add and Double Function" $ do
          
          let str = "doubleSum x y :: I32 -> I32 -> I32 :=\n  sum :: I32 $= add_i32 x y\n  sum = mul_i32 sum 2\n  return sum"
          P.function # str

main :: IO ()
main = hspec spec