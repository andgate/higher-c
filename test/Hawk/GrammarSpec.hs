module Hawk.GrammarSpec where

import SpecHelper

spec :: Spec
spec = do
    describe "Hawk.Grammar" $ do
        context "AST Generation" $ do
            it "A simple function that adds 1 to the input." $ do
                let content = "f x : Int -> Int := { x + 1; }"
                    result = []

                parse "example.hs" content `shouldBe` Right result

main :: IO ()
main = hspec spec