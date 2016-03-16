module Hawk.GrammarSpec where

import SpecHelper

spec :: Spec
spec = do
    describe "Hawk.Grammar" $ do
        context "Simple program" $ do
            it "parses exactly as-is" $ do
                let content = "f x : Int -> Int := x + 1"

                parse "" content `shouldBe` Right [TString content]

main :: IO ()
main = hspec spec