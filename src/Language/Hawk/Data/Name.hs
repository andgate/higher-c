{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Data.Name where

import Data.Ix
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Generics

-- | Name is a unique identifier
newtype Name = Name { nameId :: Int } deriving (Show, Read, Eq, Ord, Ix, Data, Typeable)

instance Enum Name where
    toEnum = Name
    fromEnum (Name n) = n

-- | return an infinite stream of 'Name's starting with @nameId@ 0
newNameSupply :: [Name]
newNameSupply = namesStartingFrom 0

-- | get the infinite stream of unique names starting from the given integer
namesStartingFrom :: Int -> [Name]
namesStartingFrom k = [Name k..]