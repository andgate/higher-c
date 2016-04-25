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
                    
                let result = Right (HkTranslUnit (HkRootModule [HkIdent "Test" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 1, spanEndRow = 1, spanStartColumn = 5, spanEndColumn = 9}}),HkIdent "Main" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 1, spanEndRow = 1, spanStartColumn = 10, spanEndColumn = 14}})] [HkExtImport (HkPrivate (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 3, spanEndRow = 3, spanStartColumn = 1, spanEndColumn = 4}})) [HkImportItem {import_item = [HkIdent "Data" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 3, spanEndRow = 3, spanStartColumn = 5, spanEndColumn = 9}}),HkIdent "List" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 3, spanEndRow = 3, spanStartColumn = 10, spanEndColumn = 14}})], import_alias = Nothing, import_annot = NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 3, spanEndRow = 3, spanStartColumn = 5, spanEndColumn = 14}}}] (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 3, spanEndRow = 3, spanStartColumn = 1, spanEndColumn = 14}}),HkExtImport (HkPrivate (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 1, spanEndColumn = 4}})) [HkImportItem {import_item = [HkIdent "Math" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 5, spanEndColumn = 9}}),HkIdent "Trig" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 10, spanEndColumn = 14}}),HkIdent "sin" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 15, spanEndColumn = 18}})], import_alias = Nothing, import_annot = NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 15, spanEndColumn = 18}}},HkImportItem {import_item = [HkIdent "Math" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 5, spanEndColumn = 9}}),HkIdent "Trig" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 10, spanEndColumn = 14}}),HkIdent "cos" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 20, spanEndColumn = 23}})], import_alias = Nothing, import_annot = NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 20, spanEndColumn = 23}}},HkImportItem {import_item = [HkIdent "Math" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 5, spanEndColumn = 9}}),HkIdent "Trig" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 10, spanEndColumn = 14}}),HkIdent "tan" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 25, spanEndColumn = 28}})], import_alias = Nothing, import_annot = NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 25, spanEndColumn = 28}}},HkImportItem {import_item = [HkIdent "Math" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 5, spanEndColumn = 9}}),HkIdent "Trig" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 10, spanEndColumn = 14}}),HkIdent "Arc" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 30, spanEndColumn = 33}}),HkIdent "asin" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 34, spanEndColumn = 38}})], import_alias = Nothing, import_annot = NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 34, spanEndColumn = 38}}},HkImportItem {import_item = [HkIdent "Math" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 5, spanEndColumn = 9}}),HkIdent "Trig" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 10, spanEndColumn = 14}}),HkIdent "Arc" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 30, spanEndColumn = 33}}),HkIdent "acos" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 40, spanEndColumn = 44}})], import_alias = Nothing, import_annot = NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 40, spanEndColumn = 44}}},HkImportItem {import_item = [HkIdent "Math" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 5, spanEndColumn = 9}}),HkIdent "Trig" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 10, spanEndColumn = 14}}),HkIdent "Arc" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 30, spanEndColumn = 33}}),HkIdent "atan" (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 46, spanEndColumn = 50}})], import_alias = Nothing, import_annot = NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 46, spanEndColumn = 50}}}] (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 4, spanEndRow = 4, spanStartColumn = 1, spanEndColumn = 50}})] (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 1, spanEndRow = 4, spanStartColumn = 1, spanEndColumn = 50}})) (NodeInfo {node_file = "example/main.hk", node_span = Span {spanStartRow = 1, spanEndRow = 4, spanStartColumn = 1, spanEndColumn = 50}})) 
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