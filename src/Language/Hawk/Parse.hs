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
import Control.Monad.Log
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Data.Bag
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Parse.Message
import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Lexer.Token (Token)
import Language.Hawk.Syntax

import qualified Data.Text as Text



-- -----------------------------------------------------------------------------
-- Parser

{-
import qualified Text.Earley as E
import Text.Earley (Report (..), Prod)
import qualified Language.Hawk.Parse.Grammar as G

parseItem :: ( MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
             , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
             , MonadIO m
             )
          => [Token] -> m ItemPs
parseItem tks =
      let
         (parses, r@(Report _ expected unconsumed)) = E.fullParses (E.parser G.toplevel) tks
      in
        case parses of
          []  -> discloseNow (_UnexpectedToken # (head unconsumed))
          [p] -> do
            logInfo =<< timestamp (_ParseSuccess # "") -- Need fill path for this message
            return p
          -- This will only happen if the grammar is wrong
          ps  -> discloseNow (_AmbiguousGrammar # ps)
-}