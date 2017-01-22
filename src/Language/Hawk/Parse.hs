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
module Language.Hawk.Parse where

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (Except, throwE, runExceptT)
import Control.Monad.Trans.State.Strict (evalState, get)
import Data.Functor.Identity (runIdentity)
import Data.Text.Lazy (Text)
import Pipes
import Text.PrettyPrint.ANSI.Leijen (pretty, Pretty, putDoc)

import qualified Data.Text.Lazy as Text
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Parse.Layout as LO
import qualified Language.Hawk.Parse.Lexer as L
import qualified Language.Hawk.Parse.Grammar as G
import qualified Language.Hawk.Syntax.Module as M
import qualified Pipes.Prelude  as Pipes
import qualified Pipes.Lift  as Pipes
import qualified Text.Earley as E

-- -----------------------------------------------------------------------------
-- Parsing functions  

program :: Package.Name -> Text -> IO M.Source
program pkgName txt = do
  let lexModl' = Pipes.evalStateP L.defPos (L.lexModl txt)
      layout' = Pipes.evalStateP LO.defState LO.layout
      (toks, ()) = runIdentity $ Pipes.toListM' $ lexModl' >-> layout'
            
  let (parses, E.Report _ needed found) =
          E.fullParses (E.parser $ G.grammar pkgName) toks
  case parses of
      parse:[] -> return parse
      _      -> error "Parsing failed"

parseTest :: Text -> IO ()
parseTest txt =
  program Package.dummyName txt >>= print . pretty
  
  
