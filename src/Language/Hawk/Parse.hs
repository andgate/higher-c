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

import Control.Lens
import Control.Lens.Internal.Zoom
import Control.Monad (mapM, (<=<))
import Control.Monad.Extra (mconcatMapM)
import Control.Monad.State (MonadState, execState)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Log
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Data.Bag
import Data.Default.Class
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable
import Data.Map.Strict
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.PrettyPrint.Leijen.Text (pretty)
import Text.Earley (Report (..), Prod)

import Language.Hawk.Lex.Result (LxResult)
import Language.Hawk.Lex.Token
import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Grammar
import Language.Hawk.Parse.Message
import Language.Hawk.Syntax
import Language.Hawk.Syntax.Definition.Source
import Language.Hawk.Syntax.Term.Source

import qualified Data.List.NonEmpty             as NE
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import qualified Language.Hawk.Lex.Result       as LxR
import qualified Text.Earley                    as E



parseMany :: ( MonadChronicle (Bag e) m, AsPsErr e
             , MonadLog (WithSeverity msg) m, AsPsMsg msg )
          => LxResult -> m (Lib Term Text)
parseMany r = do
  let toks = LxR.toList r
      f (fp, toks) = do
        r <- mconcatMapM (parse fp) toks
        logInfo (_ParseSuccess # fp)
        return r
  r' <- condemn $ mconcatMapM f toks
  logInfo (_ParseFinished # ())
  return r'


parse :: ( MonadChronicle (Bag e) m, AsPsErr e )
         => FilePath -> [Token] -> m (Lib Term Text)
parse fp toks = do
  let rs = E.fullParses (E.parser toplevel) toks
  handleParser rs

  where
    handleParser (parses, r@(Report _ expected unconsumed)) =
      case parses of
        []  -> disclose $ One (_UnexpectedToken # unconsumed)
        [p] -> return p
                   
        -- This will only happen if the grammar is wrong
        ps -> disclose $ One (_AmbiguousGrammar # () )

