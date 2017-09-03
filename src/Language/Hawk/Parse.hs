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

import Language.Hawk.Compile.State
import Language.Hawk.Parse.Decl
import Language.Hawk.Parse.Error
import Language.Hawk.Parse.Helpers (MonadParser, sc)
import Language.Hawk.Parse.Message
import Language.Hawk.Parse.Term
import Language.Hawk.Syntax

import qualified Data.List.NonEmpty     as NE
import qualified Data.Map.Lazy          as Map
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Text.Megaparsec        as P
import qualified Text.Megaparsec.Error  as P


parse :: ( MonadState s m, HasHkcState s
         , MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
         , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
         , MonadIO m
         )
         => m ()
parse =
  (traverseOf_ each (processDecls <=< parseDecl)) =<< use hkcFileTexts

  where
    processDecls =
      mapM_ processDecl
      
    processDecl = \case
      Sig n t ->
        hkcTypes . at n .= Just (Forall [] t)

      Def n e ->
        hkcDefs . at n <>= Just [e]

      DataD dd ->
        processDataDecl dd

      _ -> return ()

    parseDecl =
      handleResult . parser
      
    parser (fp, str) = P.runParser hkP fp str
    
    handleResult = either handleParseError handleSuccess
    handleSuccess m
      = do
          -- logInfo =<< timestamp (_ParseSuccess # "")
          return m


processDataDecl :: ( MonadState s m, HasHkcState s
                , MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
                , MonadLog (WithSeverity (WithTimestamp msg)) m, AsParseMsg msg
                , MonadIO m
                )
         => DataDecl -> m ()
processDataDecl dd@(DataDecl n cd) = do
  hkcDatas . at n .= Just dd
  return ()
  where
    processConDecl cd = undefined
       

-- -----------------------------------------------------------------------------
-- Error Handling

handleParseError :: ( MonadChronicle (Bag (WithTimestamp e)) m, AsParseErr e
                    , MonadIO m, Default a
                    )
            => P.ParseError Char Void -> m a
handleParseError e =
  let
    msg = pack $ P.parseErrorPretty e
  in
    discloseNow (_ParseFailed # msg)
