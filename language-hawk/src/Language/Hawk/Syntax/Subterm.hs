{-# Language  LambdaCase
            , OverloadedStrings
  #-}
module Language.Hawk.Syntax.Subterm where

import Data.Text.Prettyprint.Doc


-- Defines which memory context a term's subverse is in
-- Can either be Intuitionistic or Linear
data Subterm = TIn | TLin
  deriving (Eq, Ord, Read, Show)


instance Pretty Subterm where
  pretty = \case
    TIn   -> "*"
    TLin  -> "o"