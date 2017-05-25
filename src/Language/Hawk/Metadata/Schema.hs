{-# LANGUAGE TypeSynonymInstances, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell,TypeFamilies #-}
module Language.Hawk.Metadata.Schema where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Hawk.Metadata.CacheStatus

import qualified Language.Hawk.Syntax.OpInfo as OI


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CompilerLog
    startTime Int
    checkpoint Int
    endTime Int Maybe
    deriving Show

Package
    name Text
    srcDir Text
    UniquePackage name srcDir
    deriving Show

Module
    name Text
    qualName Text
    cacheStatus CacheStatus
    UniqueModule qualName
    deriving Show

ModulePath
    ancestor ModuleId
    descendent ModuleId
    cacheStatus CacheStatus
    UniqueModulePath ancestor descendent
    deriving Show

ModuleFile
    pkg PackageId
    assoc ModuleId
    path Text
    timestamp UTCTime
    cacheStatus CacheStatus
    UniqueModuleFile path
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


Op
    src ModuleId
    name Text
    prec Int
    assoc OI.Assoc
    deriving Show


ExprDecl
    modId ModuleId
    name Text
    op OpId
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
    op OpId
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