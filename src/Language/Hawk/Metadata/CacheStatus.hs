{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Metadata.CacheStatus where

import Database.Persist.TH

data CacheStatus = Fresh | Preserved | Stale
    deriving (Show, Read, Eq)
derivePersistField "CacheStatus"