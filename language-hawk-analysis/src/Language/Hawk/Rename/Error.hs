{-# Language OverloadedStrings
           , LambdaCase
  #-}
module Language.Hawk.Rename.Error where


import Data.Text.Prettyprint.Doc
import Data.Text (Text)



data RenameError
  = UndeclaredName Text
    deriving(Show)

instance Pretty RenameError where
    pretty = \case
        UndeclaredName n ->
            "Name not found:" <+> pretty n