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
    

VarOp
    op  OpId
    fn  ExprDefId
    
Op
    src ModuleId
    name Text
    prec Int
    assoc OI.Assoc


ExprDecl
    modId ModuleId
    name Text
    opInfo ByteString
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