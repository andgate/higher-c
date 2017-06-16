{-# LANGUAGE KindSignatures
           , DataKinds
           , StandaloneDeriving
  #-}

module Language.Hawk.Syntax.Pass where

data HkcPass (c :: Pass)
deriving instance Show (HkcPass c)
deriving instance Eq (HkcPass c)
deriving instance Ord (HkcPass c)

data Pass = Parsed | Renamed | Typechecked
deriving instance Show Pass
deriving instance Eq Pass
deriving instance Ord Pass

type HkcPs = HkcPass 'Parsed
type HkcRn = HkcPass 'Renamed
type HkcTc = HkcPass 'Typechecked