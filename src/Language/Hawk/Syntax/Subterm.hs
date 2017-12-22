module Language.Hawk.Syntax.Subterm where


-- Defines which memory context a term's subverse is in
data SubTerm = TIn | TLin
  deriving (Eq, Ord, Read, Show)