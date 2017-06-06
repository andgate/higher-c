{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Cache.Types where

import Database.Persist.TH

data CacheStatus = Fresh | Preserved | Stale
    deriving (Show, Read, Eq)
derivePersistField "CacheStatus"