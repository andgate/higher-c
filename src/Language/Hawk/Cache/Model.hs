{-# LANGUAGE TypeSynonymInstances
           , EmptyDataDecls
           , FlexibleContexts
           , GADTs
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell
           , TypeFamilies
           , FlexibleInstances
  #-}
module Language.Hawk.Cache.Model where

import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Hawk.Cache.Types

type BackendT m a = ReaderT SqlBackend m a

share [mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"
      ]
      [persistLowerCase|
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


VarSymbol sql=varsymbols
    item ItemId
    name Text
    qname Text
    deriving Show

TySymbol sql=tysymbols
    item ItemId
    name Text
    qname Text
    deriving Show


Item
    pkg PackageId
    mid ModuleId
    mfid ModuleFileId
    cacheStatus CacheStatus
    bin ByteString
    deriving Show


ItemBinPs
    bin ByteString
    deriving Show

ItemBinRn
    bin ByteString
    deriving Show

ItemBinTc
    bin ByteString
    deriving Show


ExpandedDependency
    item        ItemId
    path        Text
    isExcluded  Bool
    isQual      Bool
    hasAlias    Text Maybe
    deriving Show


Foreign
    item ItemId
    UniqueForeign item
    deriving Show

Expose
    item ItemId
    UniqueExpose item
    deriving Show

Vow
    item ItemId
    UniqueVow item
    deriving Show

TypeSig
    item ItemId
    UniqueTypeSig item
    deriving Show
    

Var
    item ItemId
    UniqueVar item
    deriving Show

Val
    item ItemId
    UniqueVal item
    deriving Show
    
    
Fun
    item ItemId
    UniqueFun item
    deriving Show
    

NewType
    item ItemId
    UniqueNewType item
    deriving Show
    

TypeAlias
    item ItemId
    UniqueTypeAlias item
    deriving Show


TypeClass
    item ItemId
    UniqueTypeClass item
    deriving Show


TypeClassInst
    item ItemId
    UniqueTypeClassInst item
    deriving Show


DataType
    item ItemId
    UniqueDataType item
    deriving Show

|]