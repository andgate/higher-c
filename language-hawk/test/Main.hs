module Main where


import Data.Either
import Data.Text
import Data.Text.IO as Text hiding(putStrLn)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Hawk.Parse  as Hk
import Language.Hawk.Lex    as Hk
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Source


srcPath :: FilePath
srcPath = "example/Example.hk"

main = do
    srcText <- Text.readFile "example/Example.hk"
    
    putStrLn "\n\nBeginning Lexxing:"
    toks <- testLex srcText

    putStrLn "\n\nBeginning Parsing:"
    ast <- testParse toks
    
    return ()


testLex :: Text -> IO [[Token]]
testLex srcText = do
    case Hk.lex srcPath srcText of
        Left err   -> do
            putDoc $ pretty err
            error "\nLexer encountered fatal error."
        
        Right srcToks -> do
            putDoc $ pretty srcToks
            return srcToks


testParse :: [[Token]] -> IO [TopLevelDef]
testParse srcToks = do
    case partitionEithers (Hk.parse srcPath <$> srcToks) of
        ([], srcAst)   -> do
            putDoc $ pretty srcAst
            return srcAst

        (errs, _)   -> do
            mapM_ putDoc (pretty <$> errs)
            error "\nParser encountered fatal error."

    