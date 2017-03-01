{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hawk.GrammarSpec where

import SpecHelper

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Yaml as YAML
import qualified Data.Yaml.Pretty as YAML
import qualified Language.Hawk.Compile as C
import qualified Language.Hawk.Metadata as MD
import qualified Language.Hawk.Parse.Helpers as P
import qualified Language.Hawk.Parse.Grammar.TopLevel as P
import qualified Language.Hawk.Report.Result as Result

spec :: Spec
spec = do
  describe "Parser" $ do
    context "Test Files" $ do
      it "Can compile" $ do
        let s = C.CompilerState "def" ["example/main.hk"] C.InitialPhase
        C.compile s
        


main :: IO ()
main = hspec spec