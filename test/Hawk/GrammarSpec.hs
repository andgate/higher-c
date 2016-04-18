module Hawk.GrammarSpec where

import SpecHelper

import Data.Sequence (fromList)

spec :: Spec
spec = do
    describe "Parsing Examples" $ do
    
        context "AST Generation" $ do
            it "example/main.hk" $ do
                ast <- parseFile "example/main.hk"
                
                putStr "\nAST Generated: "
                print ast
                    
                let result = Right (ModuleExpr [] [ModuleExpr ["Foo"] [ExternExpr "print" ["str"] ["String","Unit"],FuncExpr "bar" ["x","y"] ["Int","Int","Int"] (DoExpr [ValDecExpr "sum" ["Int"] (BinaryOpExpr "+" (VarExpr "x") (VarExpr "y")),ReturnExpr (VarExpr "sum")]),FuncExpr "baz" [] ["Int"] (BinaryOpExpr "*" (IntExpr 3) (IntExpr 2))]])
                ast `shouldBe` result
                
        context "IR Generation" $ do
            it "example/main.hk" $ do
                ast <- parseFile "example/main.hk"
                
                case ast of
                    Right ast' -> do
                        ir_str <- to_ir_string ast'
                        
                        putStr "\nIR Generated:\n"
                        print ir_str
                    
                        (length ir_str == 0)  `shouldBe` False
                    _ -> return ()


main :: IO ()
main = hspec spec