module Main where


import Data.Text
import Data.Text.IO as Text hiding(putStrLn, getLine)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Hawk.Parse  as Hk
import Language.Hawk.Lex    as Hk
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
            putDoc $ pretty srcToks <> line
            return srcToks


testParse :: [Token] -> IO SrcModule
testParse srcToks = do
    case Hk.parse srcPath srcToks of
        Right srcAst   -> do
            putDoc $ pretty srcAst <> line
            return srcAst

        Left err       -> do
            putDoc $ pretty err <> line
            error "\nParser encountered fatal error."

    