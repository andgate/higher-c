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

import Language.Hawk.Lex.Token
import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Grammar
import Language.Hawk.Parse.Message
import Language.Hawk.Syntax

import qualified Data.List.NonEmpty     as NE
import qualified Data.Map.Lazy          as Map
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Text.Earley            as E


parse :: ( MonadChronicle (Bag e) m, AsPsErr e
         , MonadLog (WithSeverity msg) m, AsPsMsg msg
         )
         => (FilePath, [Token]) -> m (FilePath, Decl)
parse (fp, toks) = do
  let rs = E.fullParses (E.parser toplevel) toks
  handleResult rs

  where
    handleResult (parses, r@(Report _ expected unconsumed)) =
      case parses of
        []  -> disclose $ One (_UnexpectedToken # unconsumed)
        [p] -> do logInfo (_ParseSuccess # fp) -- Need fullpath for this message
                  return (fp, p)
                   
        -- This will only happen if the grammar is wrong
        ps -> disclose $ One (_AmbiguousGrammar # ps)

