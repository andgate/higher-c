{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Syntax.Signature where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc


data Sig t
  = Sig
      { _sigName :: Text
      , _sigType :: t
      }
    deriving (Show, Eq)


instance (Pretty t) => Pretty (Sig t) where
  pretty (Sig n t) =
    pretty n
      <+> "::"
      <+> pretty t      
