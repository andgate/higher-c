module Hawk.GrammarSpec where

import SpecHelper

import Data.Sequence (fromList)


-- ASTs must be imported qualifed, and thus need to be imported here
-- import qualified Language.Hawk.Syntax.AST as Syn
-- import qualified Language.Hawk.Core.AST as Core

spec :: Spec
spec = do
    describe "Parsing Examples" $ do
    
        context "AST Generation" $ do
            it "example/grammar.hk" $ do
                ast <- parseFile "example/grammar.hk"
                
                putStr "\nAST Generated: "
                print ast
                    
                let result = error "no valid result"
                ast `shouldBe` result
          
        context "IR Generation" $ do
            it "example/main.hk" $ do
                syn_ast <- parseFile "example/main.hk"
                
                case syn_ast of
                    Right syn_ast' -> do
                        let core_ast = emit syn_ast'
                        ir_str <- to_ir_string core_ast
                        
                        putStr "\nIR Generated:\n"
                        print ir_str
                    
                        (length ir_str == 0)  `shouldBe` False
                    _ -> return ()


main :: IO ()
main = hspec spec