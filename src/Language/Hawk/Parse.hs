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
import Language.Hawk.Parse.Document
import Language.Hawk.Report.Result
import Text.Earley (Report (..), Prod)
import Text.PrettyPrint.ANSI.Leijen (pretty)

import qualified Data.Text as Text
import qualified Language.Hawk.Parse.Lexer.Layout as LO
import qualified Language.Hawk.Parse.Lexer.Token as Tok
import qualified Language.Hawk.Parse.Grammar as G
import qualified Language.Hawk.Report.Info as Info
import qualified Language.Hawk.Report.Error as Err
import qualified Text.Earley as E


-- -----------------------------------------------------------------------------
-- Parser

itemParser :: MonadIO m => Conduit TokenDoc m (Result DocItem)
itemParser = awaitForever go
  where
    go :: MonadIO m => TokenDoc -> Conduit TokenDoc m (Result DocItem)
    go (Doc mid fp toks) = do
      let (parses, r@(Report _ expected unconsumed)) =
              E.fullParses (E.parser G.toplevel) toks

      yield $
        case parses of
            []  -> throw $ Err.Parse (head unconsumed)
            [p] -> return $ Doc mid fp p
            -- This will only happen is the grammar is wrong
            ps  -> error $ "BUG FOUND: " ++ show (length ps) ++ " possible parses found.\n\n" ++ show (map pretty ps)
