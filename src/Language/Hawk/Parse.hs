-- This parser is based on the haskell Earley package. 
-- While it sacrifices speed for power, the earley algorithm
-- is one of the best modern parsing algorithims out there.
-- While an unpopular choice for compilers, it is thanks
-- to the Earley algorithm that Hawk can have a more 
-- expressive grammar than other languages.
--
-- The parser takes input in the form of a token stream.
-- Pipes is used for streaming. Whitespace layout formatting
-- is actually a pipe between the lexer and the parser,
-- that filters the token stream and outputs layout tokens
-- when necessary.
--
{-# LANGUAGE RankNTypes #-}
module Language.Hawk.Parse where

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (Except, throwE, runExceptT)
import Control.Monad.Trans.State.Strict (evalState, get)
import Data.Either.Unwrap (fromLeft, fromRight)
import Data.Functor.Identity (runIdentity)
import Data.Text.Lazy (Text)
import Language.Hawk.Parse.Helpers (defTypeOps, defExprOps, ExprOpTable, TypeOpTable)
import Pipes
import Text.Earley (Report (..), Prod)
import Text.Earley.Mixfix (Holey, Associativity)
import Text.PrettyPrint.ANSI.Leijen (pretty, Pretty, putDoc)

import qualified Data.Text.Lazy as Text
import qualified Language.Hawk.Parse.Layout as LO
import qualified Language.Hawk.Parse.Lexer as L
import qualified Language.Hawk.Parse.Grammar as G
import qualified Language.Hawk.Syntax.Module as M
import qualified Pipes.Prelude  as Pipes
import qualified Pipes.Lift  as Pipes
import qualified Text.Earley as E
import qualified Text.Earley.Mixfix as E


-- -----------------------------------------------------------------------------
-- Parser
parse :: TypeOpTable  -- type symbol table
      -> ExprOpTable    -- var symbol table
      -> Text -- Source input
      -> IO M.Source
parse typOps exprOps txt = do
  let lexModl' = Pipes.evalStateP L.defState (L.lexModl txt)
      layout' = Pipes.evalStateP LO.defState LO.layout
      (toks, ()) = runIdentity $ Pipes.toListM' $ lexModl' >-> layout'
      
  print toks
            
  let (parses, r@(Report _ needed found)) =
          E.fullParses (E.parser $ G.grammar typOps exprOps) toks
  
  case parses of
      []       -> error $ "No parses found.\n" ++ show r
      p:[]      -> return p
      ps        -> error $ show (length ps) ++ " possible parses found.\n\n" ++ show (map pretty ps)


mangledParse :: Text -> IO M.Source
mangledParse txt =
  parse defTypeOps defExprOps txt

-- -----------------------------------------------------------------------------
-- Test Parser
parseTest :: Text -> IO ()
parseTest txt =
  parse defTypeOps defExprOps txt >>= print . pretty
