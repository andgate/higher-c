-- This parser is based on the haskell Earley package. 
-- While it sacrifices speed for power, the earley algorithm
-- is one of the best modern parsing algorithims out there.
-- While an unpopular choice for compilers, it is thanks
-- to the Earley algorithm that Hawk can have a more 
-- expressive grammar than other languages.
--
-- The parser takes input in the form of a token stream.
-- 
module Language.Hawk.Parse where

import Text.PrettyPrint.ANSI.Leijen (pretty, Pretty, putDoc)

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Grammar
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Syntax.Module as Module

-- -----------------------------------------------------------------------------
-- Parsing functions  

program :: Package.Name -> Text -> IO M.Source
program text = evalState (runExceptT m) (L.P 1 0)
  where
    m = do
        (locatedTokens, mtxt) <- lift (Pipes.toListM' (L.lexExpr text))
        case mtxt of
            Nothing  -> return ()
            Just txt -> error "Lex failed"
        let (parses, Report _ needed found) =
                fullParses (parser grammar) locatedTokens
        case parses of
            parse:[] -> return parse
            _      -> error "Parsing failed"

parseTest :: Text -> IO ()
parseTest =
  print . pretty . program
  
  
