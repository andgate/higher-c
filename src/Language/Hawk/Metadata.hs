{-# LANGUAGE TypeSynonymInstances, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell,TypeFamilies #-}
module Language.Hawk.Metadata where


import Data.Binary (encode, decode)

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe
import Data.Text.Lazy (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import qualified Language.Hawk.Syntax.Alias as A
import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Function as F
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Module as M
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Record as R
import qualified Language.Hawk.Syntax.Variable as V


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MModule
    name Text
    source Text
    deps [Text]
    depIds [MModuleId] -- Generated after all modules are loaded
    deriving Show
    
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
    typesig ByteString Maybe
    deriving Show
    
MVarItem
    modId MModuleId
    bindingId MBindingId
    typesig ByteString Maybe
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


store :: M.Source -> Text -> IO ()
store (M.Module n its) src = runSqlite ":memory:" $ do
  runMigration migrateAll
  modId <- insert $ MModule n src (I.getDeps its) []
  mapM_ (storeItem modId) its
  
  where
    storeItem modId i =
      case i of
        I.Import _ n -> return () -- Handled later
        I.Function _ f -> storeFn modId f
        I.Variable _ v -> storeVar modId v
        I.Record _ r -> storeRec modId r
        I.Alias _ a -> storeAlias modId a
        
      
    storeBinding (B.Binding m (N.Name _ n)) =
      insert $ MBinding n (B.isRef m) (B.isMut m)
        
    storeFn modId (F.Function oi (N.Name _ n) args t b) = do
      let tdat = toStrict $ encode t
      let oidat = toStrict $ encode oi
      argIds <- mapM storeBinding args
      insert_ $ MFnItem modId n oidat argIds (Just tdat)
      
    storeVar modId (V.Variable n t b) = do
      let tdat = toStrict $ encode t
      bindingId <- storeBinding n
      insert_ $ MVarItem modId bindingId (Just tdat)
    
    storeRec modId (R.Record (N.Name _ n) fs) = do
      recId <- insert $ MRecItem modId n []
      fldIds <- mapM (storeRecField recId) fs
      update recId [MRecItemFieldIds =. fldIds]
      
    storeRecField recId (R.RecordField (N.Name _ n) t) = do
      let tdat = toStrict $ encode t
      insert $ MRecField recId n tdat
    
    storeAlias modId (A.Alias (N.Name _ n) t) = do
      let tdat = toStrict $ encode t
      insert_ $ MAliasItem modId n tdat