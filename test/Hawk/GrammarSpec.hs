{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hawk.GrammarSpec where

import Language.Hawk.Parse (parseTest)
import SpecHelper

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Yaml as YAML
import qualified Data.Yaml.Pretty as YAML
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Compile as Compile
import qualified Language.Hawk.Parse.Helpers as P
import qualified Language.Hawk.Parse.Grammar as P
import qualified Language.Hawk.Report.Result as Result

spec :: Spec
spec = do
  describe "Parser" $ do
    context "Test Files" $ do
      it "Can parse example/main.hk" $ do
          str <- Text.readFile "example/main.hk"
          parseTest str

main :: IO ()
main = hspec spec