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
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Compile.State
import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Helpers (MonadParser, ParserOpTable, mkParserOpTable, splitLinefolds)
import Language.Hawk.Parse.Item
import Language.Hawk.Parse.Message
import Language.Hawk.Parse.OpTable
import Language.Hawk.Parse.State
import Language.Hawk.Parse.Lexer.Token (Token)
import Language.Hawk.Syntax

import qualified Data.List.NonEmpty     as NE
import qualified Data.Map.Lazy          as Map
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Text.Megaparsec.Prim   as P
import qualified Text.Megaparsec.Error  as P


parse :: ( MonadState s m, HasHkcState s, HasParseState s
         , MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
         , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
         , MonadIO m
         )
         => m ()
parse = do 
  parseFilesplit
  parseOpTable
  --ops <- uses psOps mkParserOpTable
  --items <- (traverseOf each (parseItem ops) =<< use psToks)
  --return ()

-- -----------------------------------------------------------------------------
-- Operator table parsing

parseFilesplit :: ( MonadState s m, HasParseState s
                  , MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
                  , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
                  , MonadIO m
                  )
        => m ()
parseFilesplit = do
  tss <- use psToks
  psToks <~ (foldrM handleToks [] tss)

  where
    parse = P.runParser splitLinefolds ""

    handleToks toks acc =
      let r = parse toks
      in handleResult r acc

    handleResult r acc = either (\err -> handleParseError err) 
                                (\toks -> return (toks ++ acc))
                                r


parseOpTable :: ( MonadState s m, HasParseState s
                , MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
                , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
                , MonadIO m
                )
         => m ()
parseOpTable =
  psToks <~ (foldrM handleToks [] =<< use psToks)

  where
    parseFixity = P.runParser fixityP ""
    
    handleToks toks acc =
      let r = parseFixity toks
      in handleResult acc r

    handleResult acc = either (\err -> handleParseError err) 
                              (handleSuccess acc)
    handleSuccess acc =
      either (\toks -> return (toks:acc))
             (\ops -> mapM_ insertOp ops >> return acc)


insertOp :: ( MonadState s m, HasParseState s
            , MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
            , MonadIO m
            )
         => Operator -> m ()
insertOp o = do
  let p = o^.opPrec
  checkPrecedence p
  psOps . at p . _Just %= (o:)


-- This should at least be in Chronicle
checkPrecedence :: ( MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
                   , MonadIO m
                   )
                => Int -> m ()
checkPrecedence x
  | x < 0 = discloseNow (_FixityTooLow # x) -- Todo: get location information
  | x > 9 = discloseNow (_FixityTooHigh # x)
  | otherwise = return () -- fixity was just right

-- -----------------------------------------------------------------------------
-- Item Parsing

parseItem :: ( MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
             , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
             , MonadIO m
             )
         => ParserOpTable -> [Token] -> m Item
parseItem ops ts
  = either handleParseError handleSuccess result
  where
    result = P.runParser (itemP ops) "" ts
    
    handleSuccess m
      = do
          logInfo =<< timestamp (_ParseSuccess # "")
          return m


-- -----------------------------------------------------------------------------
-- Error Handling

handleParseError :: ( MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
                    , MonadIO m, Default a
                    )
            => P.ParseError Token P.Dec -> m a
handleParseError (P.ParseError _ unexpected _ _)
  = case Set.toList unexpected of
      ((P.Tokens (t:|_)):_)
        -> discloseNow (_UnexpectedToken # t)

      _ -> discloseNow (_UnexpectedParseErr # ())
