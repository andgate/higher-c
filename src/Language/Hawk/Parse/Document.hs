module Language.Hawk.Parse.Document where

import Data.Text (Text)
import Language.Hawk.Metadata.Schema (ModuleId)
import Language.Hawk.Parse.Lexer.Token (Token)
import System.FilePath (FilePath)

import qualified Language.Hawk.Syntax.Item as I

data Document a =
  Doc ModuleId FilePath a
  deriving Show

type InfoDoc = Document ()
type TextDoc = Document Text
type TokenDoc = Document [Token]
type DocItem = Document I.Source