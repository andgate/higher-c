{-# LANGUAGE TemplateHaskell
           , DeriveFunctor
  #-}
module Language.Hawk.Parse.Document where

import Control.Lens
import Data.Text (Text)
import Language.Hawk.Cache.Model (PackageId, ModuleId, ModuleFileId)
import Language.Hawk.Parse.Lexer.Token (Token)
import System.FilePath (FilePath)
import Text.PrettyPrint.ANSI.Leijen (pretty, (<+>))

import Language.Hawk.Syntax

import qualified Text.PrettyPrint.ANSI.Leijen     as PP

data Document a =
  Doc 
  { _docPkg :: PackageId
  , _docMod :: ModuleId
  , _docFile :: ModuleFileId
  , _docPath :: FilePath
  , _docData :: a
  }
  deriving (Show, Functor)

type InfoDoc = Document ()
type TextDoc = Document Text
type TokenDoc = Document [Token]
type DocItem = Document SrcItem

makeLenses ''Document