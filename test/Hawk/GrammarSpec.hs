module Hawk.GrammarSpec where

import SpecHelper
import Debug.Trace

spec :: Spec
spec = do
    describe "Parsing Examples" $ do
        context "AST Generation" $ do
            it "example/main.hk" $ do
                ast <- parseFile "example/main.hk"
                
                putStr "\nAST Generated: "
                print ast
                    
                let result = Right (ModuleExpr [] [ModuleExpr ["foo"] [FuncExpr "bar" ["x"] ["Unit","Int"] (PlusExpr (IntExpr 1) (IntExpr 1))]])
                ast `shouldBe` result

main :: IO ()
main = hspec spec