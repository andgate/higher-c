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
import Control.Monad (mapM)
import Control.Monad.State (MonadState)
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
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Helpers (ParserOpTable, mkParserOpTable)
import Language.Hawk.Parse.Item
import Language.Hawk.Parse.Item.Monad
import Language.Hawk.Parse.Item.Types
import Language.Hawk.Parse.Module
import Language.Hawk.Parse.Message
import Language.Hawk.Parse.Lexer.Token (Token)
import Language.Hawk.Syntax

import qualified Data.List.NonEmpty     as NE
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Text.Megaparsec.Prim   as P
import qualified Text.Megaparsec.Error  as P


parse :: ( MonadState s m, HasSrcMod s
         , MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
         , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
         , MonadIO m
         )
         => [(FilePath, [Token])] -> m ()
parse xs = do 
  -- m is unparsed
  mapM_ parseMod xs
  -- handle operator imports here
  -- Traverse each module scope, generate an op table
  -- parse each module scope's items, and then produce
  -- a new module scope using the generated items
  m <- use srcMod
  srcMod <~ transformM parseModItems m
  
  where
    parseModItems m = do
      let scopes = m ^. modScopes
      scopes' <- mapM parseMScopeItems scopes
      return (m & modScopes .~ scopes')

    parseMScopeItems s = do
      let fp    = s^.mscopePath
          ops   = mkParserOpTable (s^.mscopeOps)
          toks  = s^.mscopeToks
      items <- mapM (parseItem fp ops) toks
      return (s & mscopeItems .~ items)


-- -----------------------------------------------------------------------------
-- Module Parsing
-- | Parses module structurre, fills it with unparsed items,
-- | and builds operator sets.

parseMod :: ( MonadState s m, HasSrcMod s
            , MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
            , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
            , MonadIO m
            )
         => (FilePath, [Token]) -> m ()
parseMod (fp, ts)
  = (srcMod <>=) =<< either (handleParseError fp) handleSuccess result
  where
    result = P.runParser (modP fp) fp ts
    
    handleSuccess m
      = do
          logInfo =<< timestamp (_ParseSuccess # fp)
          return m

-- -----------------------------------------------------------------------------
-- Item Parsing

parseItem :: ( MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
             , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
             , MonadIO m
             )
         => FilePath -> ParserOpTable -> [Token] -> m Item
parseItem fp ops ts
  = either (handleParseError fp) handleSuccess result
  where
    result = runIdentity $ runItemParserT itemP fp ops ts
    
    handleSuccess m
      = do
          logInfo =<< timestamp (_ParseSuccess # fp)
          return m


-- -----------------------------------------------------------------------------
-- Error Handling

handleParseError :: ( MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
                    , MonadIO m, Default a
                    )
            => FilePath -> P.ParseError Token P.Dec -> m a
handleParseError fp (P.ParseError _ unexpected _ _)
  = case Set.toList unexpected of
      ((P.Tokens (t:|_)):_)
        -> discloseNow (_UnexpectedToken # t)

      _ -> discloseNow (_UnexpectedParseErr # fp)
