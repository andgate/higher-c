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
                    
                let result = Right undefined
                ast `shouldBe` result
          
        {-      
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
                    
        -}


main :: IO ()
main = hspec spec