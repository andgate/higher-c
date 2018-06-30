{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Syntax.Signature where

import Data.Text (Text)
import qualified Text.PrettyPrint.Leijen.Text as PP


data Sig t
  = Sig
      { _sigName :: Text
      , _sigType :: t
      }
    deriving (Show, Eq)


instance (PP.Pretty t) => PP.Pretty (Sig t) where
  pretty (Sig n t) =
    PP.textStrict n
      PP.<+> PP.textStrict "::"
      PP.<+> PP.pretty t      
