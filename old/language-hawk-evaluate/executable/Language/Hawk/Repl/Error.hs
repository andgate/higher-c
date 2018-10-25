{-# Language LambdaCase #-}
module Language.Hawk.Repl.Error where

import Data.Text.Prettyprint.Doc
import Language.Hawk.Lex.Error
import Language.Hawk.Parse.Error
import Language.Hawk.Rename.Error


data ReplError
  = ReplParseErr ParseError
  | ReplRenameErr RenameError
  | ReplLexErr LexError

instance Pretty ReplError where
    pretty = \case
        ReplParseErr err -> pretty err
        ReplRenameErr err -> pretty err
        ReplLexErr err -> pretty err