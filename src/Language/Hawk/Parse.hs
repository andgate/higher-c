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
{-# LANGUAGE  FlexibleContexts #-}
module Language.Hawk.Parse where

import Control.Lens
import Control.Monad.Chronicle
import Text.Earley (Report (..), Prod)
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Lexer.Token (Token)
import Language.Hawk.Syntax

import qualified Data.Text as Text

import qualified Language.Hawk.Parse.Grammar as G
import qualified Text.Earley as E


-- -----------------------------------------------------------------------------
-- Parser

parse :: (MonadChronicle [e] m, AsParseErr e)
      => [Token] -> m [ItemPs]
parse toks =
      let
        (parses, r@(Report _ expected unconsumed)) =
            E.fullParses (E.parser G.toplevel) toks
      in
        case parses of
            []  -> disclose [_UnexpectedToken # (head unconsumed)]
            [p] -> return p
            -- This will only happen is the grammar is wrong
            ps  -> disclose [_AmbiguousGrammar # ps]
