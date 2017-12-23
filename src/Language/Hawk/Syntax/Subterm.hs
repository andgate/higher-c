{-# Language  LambdaCase
            , OverloadedStrings
  #-}
module Language.Hawk.Syntax.Subterm where

import qualified Text.PrettyPrint.Leijen.Text as PP


-- Defines which memory context a term's subverse is in
data SubTerm = TIn | TLin
  deriving (Eq, Ord, Read, Show)


instance PP.Pretty SubTerm where
  pretty = \case
    TIn -> PP.textStrict "*"
    TLin -> PP.textStrict "o"