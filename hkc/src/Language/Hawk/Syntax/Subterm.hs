{-# Language  LambdaCase
            , OverloadedStrings
  #-}
module Language.Hawk.Syntax.Subterm where

import qualified Text.PrettyPrint.Leijen.Text as PP


-- Defines which memory context a term's subverse is in
-- Can either be Intuitionistic or Linear
data Subterm = TIn | TLin
  deriving (Eq, Ord, Read, Show)


instance PP.Pretty Subterm where
  pretty = \case
    TIn -> PP.textStrict "*"
    TLin -> PP.textStrict "o"