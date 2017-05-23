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

import Conduit
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (Except, throwE, runExceptT)
import Data.Either.Unwrap (fromLeft, fromRight)
import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Language.Hawk.Parse.Document
import Language.Hawk.Parse.Helpers (defTypeOps, defExprOps, ExprOpTable, TypeOpTable)
import Language.Hawk.Parse.Lexer.Token (Token)
import Text.Earley (Report (..), Prod)
import Text.Earley.Mixfix (Holey, Associativity)
import Text.PrettyPrint.ANSI.Leijen (pretty, Pretty, putDoc)

import qualified Data.Text as Text
import qualified Language.Hawk.Parse.Lexer.Layout as LO
import qualified Language.Hawk.Parse.Grammar.TopLevel as G
import qualified Language.Hawk.Parse.Grammar.ExprLevel as G
import qualified Language.Hawk.Parse.Grammar.TypeLevel as G
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Module as M
import qualified Text.Earley as E
import qualified Text.Earley.Mixfix as E


-- -----------------------------------------------------------------------------
-- Parser

itemParser :: MonadIO m => Conduit TokenDoc m DocItem
itemParser = awaitForever go
  where
    go :: MonadIO m => TokenDoc -> Conduit TokenDoc m DocItem
    go (Doc mid fp toks) = do 
      let (parses, r@(Report _ needed found)) =
              E.fullParses (E.parser $ G.toplevel) toks

      case parses of
          []      -> error $ "No parses found.\n" ++ show r
          p:[]    -> yield (Doc mid fp p)
          ps      -> error $ show (length ps) ++ " possible parses found.\n\n" ++ show (map pretty ps)