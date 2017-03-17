{-# LANGUAGE TypeSynonymInstances, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell,TypeFamilies #-}
module Language.Hawk.Metadata.Schema where

import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import qualified Language.Hawk.Syntax.OpInfo as OI
import qualified Language.Hawk.Compile.Monad as C

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CompilerLog
    phase C.CompilerPhase
    startTime Int
    checkpoint Int
    endTime Int Maybe

Package
    name Text

Module
    pkgId PackageId
    name Text
    source Text
    deriving Show
    
ModuleImport
    modId   ModuleId
    path    [Text]
    target  Text
    
ModuleExport
    modId   ModuleId
    path    [Text]
    target  Text

Name
    name Text
    maybePos PositionId Maybe
    deriving Show
    
Position
    linenum Int
    colnum Int
    deriving Show
    
-- Namespace data
TopLevelName
    modId ModuleId
    nameId NameId
    deriving Show
    
-- Module subscriptions
ModuleRelation
    publisher ModuleId
    subscriber  ModuleId
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