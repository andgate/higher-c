{-# LANGUAGE TypeSynonymInstances, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell,TypeFamilies #-}
module Language.Hawk.Metadata.Schema where

import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import qualified Language.Hawk.Syntax.OpInfo as OI

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Module
    name Text
    source Text
    deps [Text]
    depIds [ModuleId] -- Generated after all modules are loaded
    deriving Show

TypeOp
    op  OpId
    td TypeDefId

VarOp
    op  OpId
    fn  ExprDefId
    
Op
    src ModuleId
    name Text
    prec Int
    assoc OI.Assoc
    
ExprDef
    modId ModuleId
    name Text
    opInfo ByteString
    typesig ByteString -- This field may not get added until type-inference
    body ByteString -- This field may not be set until the expressions are demangled
    deriving Show
    
TypeDef
    modId ModuleId
    ctx ByteString
    name Text
    vars [Text]
    body ByteString
    deriving Show    
    
Record
    modId ModuleId
    name Text
    body [RecordFieldId]
    deriving Show
    
RecordField
    parentId RecordId
    name Text
    typesig ByteString
    deriving Show
    
    
ClassDef
    modId ModuleId
    name Text
    vars [Text]
    body [ClassDefFieldId]
    deriving Show
    
ClassDefField
    parentId ClassDefId
    name Text
    typesig ByteString
    deriving Show
    
    
ClassInst
    modId ModuleId
    name Text
    vars [ByteString]
    body [ClassInstFieldId]
    deriving Show
    
ClassInstField
    parentId ClassInstId
    def ExprDef
    deriving Show
|]