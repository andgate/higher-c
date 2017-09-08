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
{-# LANGUAGE  FlexibleContexts 
            , TypeFamilies 
            , GeneralizedNewtypeDeriving
            , FlexibleInstances
            , MultiParamTypeClasses
            , TupleSections
            , LambdaCase
  #-}
module Language.Hawk.Parse where

import Prelude hiding (lex)

import Control.Lens
import Control.Lens.Internal.Zoom
import Control.Monad (mapM, (<=<))
import Control.Monad.State (MonadState, execState)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Log
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Data.Bag
import Data.Default.Class
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.PrettyPrint.Leijen.Text (pretty)
import Text.Earley (Report (..), Prod)

import Language.Hawk.Compile.State
import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Grammar
import Language.Hawk.Parse.Message
import Language.Hawk.Parse.Token
import Language.Hawk.Parse.Lex (lexer)
import Language.Hawk.Parse.Lex.Error
import Language.Hawk.Syntax

import qualified Data.List.NonEmpty     as NE
import qualified Data.Map.Lazy          as Map
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Text.Earley            as E


parse :: ( MonadState s m, HasHkcState s
         , MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e, AsLexErr e
         , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
         , MonadIO m
         )
         => [Token] -> m Decl
parse toks = do
  let rs = E.fullParses (E.parser toplevel) toks
  handleResult rs

  where
    handleResult (parses, r@(Report _ expected unconsumed)) =
      case parses of
        []  -> discloseNow (_UnexpectedToken # unconsumed)
        [p] -> do logInfo =<< timestamp (_ParseSuccess # (head toks ^. locPath)) -- Need fill path for this message
                  return p
                   
        -- This will only happen if the grammar is wrong
        ps -> discloseNow (_AmbiguousGrammar # ps)


processDataDecl :: ( MonadState s m, HasHkcState s
                , MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
                , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
                , MonadIO m
                )
         => DataDecl -> m ()
processDataDecl dd@(DataDecl n cd) = do
  hkcDatas . at n .= Just dd
  where
    processConDecl cd = undefined       

