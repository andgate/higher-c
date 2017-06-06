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
    name Text
    maybePos PositionId Maybe
    deriving Show
    
Position
    linenum Int
    colnum Int
    deriving Show
 

Dependency
    modId   ModuleId
    isQual  Bool
    target  ByteString
    alias   Text Maybe
    deriving Show


ExprDecl
    modId ModuleId
    name Text
    typesig ByteString
    deriving Show
    
    
ExprDef
    modId ModuleId
    decId ExprDeclId
    body ByteString
    deriving Show
    
    
TypeDecl
    modId ModuleId
    name Text
    ctx ByteString
    vars [ByteString]
    deriving Show    
    
    
AliasDef
    modId ModuleId
    decId TypeDeclId
    tipe ByteString
    deriving Show
    
    
DataDef
    modId ModuleId
    decId TypeDeclId
    body [DataConId]
    deriving Show
    
DataCon
    modId ModuleId
    dataId DataDefId
    name Text
    tipe ByteString
    rows [DataRowId]
    deriving Show
    
DataRow
    modId ModuleId
    dataId DataDefId
    conId DataConId
    name Text Maybe
    tipe ByteString
    deriving Show
    
    
TypeClassDef
    modId ModuleId
    decId TypeDeclId
    body [ExprDefId]
    deriving Show
|]