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
  #-}
module Language.Hawk.Parse where

import Control.Lens
import Control.Monad.Identity (runIdentity)
import Control.Monad.Log
import Control.Monad.Chronicle
import Control.Monad.Chronicle.Extra
import Data.Bag
import Data.Default.Class
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import Data.Text (Text, pack)
import Text.PrettyPrint.Leijen.Text (pretty)

import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Helpers ()
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


-- -----------------------------------------------------------------------------
-- Module Parsing

parseMod :: ( MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
             , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
             , MonadIO m
             )
         => (FilePath, [Token]) -> m ModPs
parseMod (fp, ts)
  = either (handleParseError fp) handleSuccess result
  where
    result = P.runParser (modP fp) fp ts
    
    handleSuccess m
      = do
          logInfo =<< timestamp (_ParseSuccess # fp)
          return m

-- -----------------------------------------------------------------------------
-- Definition Parsing

parseDef :: ( MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
             , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
             , MonadIO m
             )
         => (GlobalInfo, [Token]) -> m ItemPs
parseDef (g, ts)
  = either (handleParseError fp) handleSuccess result
  where
    fp = g^.gFilePath
    result = runIdentity $ runItemParserT itemP g ts
    
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
