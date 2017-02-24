{-# LANGUAGE TypeSynonymInstances, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell,TypeFamilies #-}
module Language.Hawk.Metadata.Schema where

import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import qualified Language.Hawk.Syntax.OpInfo as OI

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MModule
    name Text
    source Text
    deps [Text]
    depIds [MModuleId] -- Generated after all modules are loaded
    deriving Show

MTypeOp
    op  MOpId
    rec MRecItemId

MVarOp
    op  MOpId
    fn  MFnItemId
    
MOp
    src MModuleId
    name Text
    prec Int
    assoc OI.Assoc
    
MBinding
    name Text
    isRef Bool
    isMut Bool
    deriving Show
    
MFnItem
    modId MModuleId
    name Text
    opInfo ByteString
    params [MBindingId]
    typesig ByteString Maybe -- This field may not get added until type-inference
    body ByteString Maybe -- This field may not be set until the expressions are demangled
    deriving Show
    
MVarItem
    modId MModuleId
    bindingId MBindingId
    typesig ByteString Maybe
    body ByteString Maybe
    deriving Show
    
MRecItem
    modId MModuleId
    name Text
    fieldIds [MRecFieldId]
    deriving Show
    
MRecField
    recId MRecItemId
    name Text
    typesig ByteString
    deriving Show
    
MAliasItem
    modId MModuleId
    name Text
    typesig ByteString
    deriving Show
|]