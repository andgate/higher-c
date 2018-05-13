module Main where

import Control.Monad
import Data.List (null)
import Data.Text hiding (null)
import Data.Text.IO as Text hiding(putStrLn, getLine)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Hawk.Parse  as Hk
import Language.Hawk.Lex    as Hk
import Language.Hawk.Lex.LFCut
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Source


srcPath :: FilePath
srcPath = "example/Example.hk"

main :: IO ()
main = do
    print "\n\nReading Example Source:"
    srcText <- Text.readFile "example/Example.hk"
    
    putStrLn "\n\nBeginning Lexing:"
    toks <- testLex srcText

    putStrLn "\n\nBeginning Parsing:"
    ast <- testParse toks
    
    return ()


testLex :: Text -> IO [Token]
testLex srcText = do
    case Hk.lex srcPath srcText of
        Left err   -> do
            putDoc $ pretty err <> line
            error "\nLexer encountered fatal error."
        
        Right srcToks -> do
            putDoc $ vcat (fmap pretty . lfCut $ srcToks) <> line
            return srcToks


testParse :: [Token] -> IO [TopLevelDef]
testParse srcToks = do
    let (errs, defs) = Hk.parse srcPath srcToks

    putStrLn "\n\nDefinitions Parsed:"
    mapM_ (\def -> putDoc $ pretty def <> line) defs

    putStrLn "\n\nErrors Parsing:"
    mapM_ (\err -> putDoc $ pretty err <> line) errs

    unless (null errs)
           (error "\nParser encountered fatal error.")

    return defs