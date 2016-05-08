module Language.Hawk.Parse.Helpers where

import qualified Data.Map as Map
import Control.Monad.State (State)
import Text.Parsec hiding (newline, spaces, State)
import Text.Parsec.Indent (indented, runIndent)
import qualified Text.Parsec.Token as T


type OpTable = Map.Map String (Int, Decl.Assoc)
type SourceM = State SourcePos
type IParser a = ParsecT String OpTable SourceM a

iParse :: IParser a -> String -> Either ParseError a
iParse parser source =
  iParseWithTable "" Map.empty parser source
  
iParseWithTable :: SourceName -> OpTable -> IParser a -> String -> Either ParseError a
iParseWithTable sourceName table aParser input =
  runIdent sourcename $ runParserT aParser table sourceName input