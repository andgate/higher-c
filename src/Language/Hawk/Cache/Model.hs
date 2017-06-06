{-# LANGUAGE TypeSynonymInstances, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell,TypeFamilies #-}
module Language.Hawk.Cache.Model where

import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Hawk.Cache.Types

type BackendT m a = ReaderT SqlBackend m a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Package
    name Text
    srcDir Text
    UniquePackage name srcDir
    deriving Show

Module
    pkg PackageId
    name Text
    qualName Text
    cacheStatus CacheStatus
    UniqueModule pkg qualName
    deriving Show

ModulePath
    pkg PackageId
    ancestor ModuleId
    descendent ModuleId
    cacheStatus CacheStatus
    UniqueModulePath pkg ancestor descendent
    deriving Show

ModuleFile
    pkg PackageId
    assoc ModuleId
    path Text
    timestamp UTCTime
    cacheStatus CacheStatus
    isBuilt Bool
    UniqueModuleFile pkg assoc path
    deriving Show


Name
    pkg PackageId
    mod ModuleId
    cacheStatus CacheStatus
    name Text
    pos ByteString
    UniqueName pkg mod name
    deriving Show
 

Dependency
    pkg PackageId
    mod ModuleId
    cacheStatus CacheStatus
    isQual  Bool
    target  ByteString
    alias   Text Maybe
    deriving Show

    
TypeSig
    pkg PackageId
    mod ModuleId
    cacheStatus CacheStatus
    name Text
    body ByteString
    deriving Show


Var
    pkg PackageId
    mod ModuleId
    cacheStatus CacheStatus
    name Text
    pos ByteString
    body ByteString
    deriving Show
    
    
Fun
    pkg PackageId
    mod ModuleId
    cacheStatus CacheStatus
    name Text
    pos ByteString
    params ByteString
    body ByteString
    deriving Show
    

NewType
    pkg PackageId
    mod ModuleId
    cacheStatus CacheStatus
    name Text
    pos ByteString
    vars ByteString
    body ByteString
    deriving Show
    

TypeAlias
    pkg PackageId
    mod ModuleId
    cacheStatus CacheStatus
    name Text
    pos ByteString
    vars ByteString
    body ByteString
    deriving Show


TypeClass
    pkg PackageId
    mod ModuleId
    cacheStatus CacheStatus
    name Text
    pos ByteString
    ctx ByteString
    vars ByteString
    body ByteString
    deriving Show


TypeClassInst
    pkg PackageId
    mod ModuleId
    cacheStatus CacheStatus
    name Text
    pos ByteString
    ctx ByteString
    args ByteString
    body ByteString
    deriving Show


DataType
    pkg PackageId
    mod ModuleId
    cacheStatus CacheStatus
    name Text
    pos ByteString
    vars ByteString
    body ByteString
    deriving Show

|]