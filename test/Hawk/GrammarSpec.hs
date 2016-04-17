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
                    
                let result = Right (ModuleExpr [] [ModuleExpr ["Foo"] [FuncDecExpr "baz" [] ["Int"] (TimesExpr (IntExpr 3) (IntExpr 2)),FuncDecExpr "bar" ["x"] ["Int","Int"] (DoExpr [ReturnExpr (VarExpr "sum"),ValDecExpr "sum" ["Int"] (PlusExpr (VarExpr "x") (VarExpr "x"))])]])
                ast `shouldBe` result

main :: IO ()
main = hspec spec