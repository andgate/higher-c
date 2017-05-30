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
import Data.Maybe (fromJust)
import Data.Text (Text)
import Language.Hawk.Parse.Document
import Language.Hawk.Parse.Helpers (defTypeOps, defExprOps, ExprOpTable, TypeOpTable)
import Language.Hawk.Parse.Lexer.Token (Token)
import Language.Hawk.Report.Result
import Text.Earley (Report (..), Prod)
import Text.Earley.Mixfix (Holey, Associativity)
import Text.PrettyPrint.ANSI.Leijen (pretty, Pretty, putDoc)

import qualified Data.Text as Text
import qualified Language.Hawk.Parse.Lexer.Layout as LO
import qualified Language.Hawk.Parse.Lexer.Token as Tok
import qualified Language.Hawk.Parse.Grammar.TopLevel as G
import qualified Language.Hawk.Parse.Grammar.ExprLevel as G
import qualified Language.Hawk.Parse.Grammar.TypeLevel as G
import qualified Language.Hawk.Report.Info as Info
import qualified Language.Hawk.Report.Error as Err
import qualified Language.Hawk.Syntax.Item as I
import qualified Text.Earley as E
import qualified Text.Earley.Mixfix as E


-- -----------------------------------------------------------------------------
-- Parser

itemParser :: MonadIO m => Conduit TokenDoc m (Result DocItem)
itemParser = awaitForever go
  where
    go :: MonadIO m => TokenDoc -> Conduit TokenDoc m (Result DocItem)
    go (Doc mid fp toks) = do
      let (parses, r@(Report _ needed found)) =
              E.fullParses (E.parser G.toplevel) toks

      yield $
        case parses of
            []  -> throw $ Err.Parse fp (fromJust . Tok.region . head $ found)
            [p] -> return $ Doc mid fp p
            -- This will only happen is the grammar is wrong
            ps  -> error $ show (length ps) ++ " possible parses found.\n\n" ++ show (map pretty ps)
