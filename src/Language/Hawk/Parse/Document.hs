module Language.Hawk.Parse.Document where

import Data.Text (Text)
import Language.Hawk.Cache.Model (ModuleId)
import Language.Hawk.Parse.Lexer.Token (Token)
import System.FilePath (FilePath)
import Text.PrettyPrint.ANSI.Leijen (pretty, (<+>))

import Language.Hawk.Syntax

import qualified Text.PrettyPrint.ANSI.Leijen     as PP

data Document a =
  Doc ModuleId FilePath a
  deriving Show

type InfoDoc = Document ()
type TextDoc = Document Text
type TokenDoc = Document [Token]
type DocItem = Document SrcItem