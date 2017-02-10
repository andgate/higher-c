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
import Language.Hawk.Compile.Monad

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


{-
  This phase collects modules and stores them into a sqlite db on disk
  for fast look-up and sharing with other projects.
-}
collect :: Compiler ()
collect = do
  collectGlobal
  buildSymbolTables
  collectExprs


{-
  Global collection consists of storing only the global information.
  This phase will make symbol information available in the database.
  This phase does not parse expressions, since it doesn't have access
  to enough information to make a symbol table.
-}
collectGlobal :: Compiler ()
collectGlobal = undefined

{-
  This is an intermediate step between global collection and expression collection.
  Once the global collection occurs, symbol information like names and operator precedence
  are then available in the database. This information can be used to generate
  expressions in the correct form.
  Symbol tables basically consist of a name and operator precedence information.
  This information is queried for each module and stored into the db in the module
  description.
-}
buildSymbolTables :: Compiler ()
buildSymbolTables = undefined

{-
  Expression collection is run after symbol tables are generated.
  These symbol tables are used to parse expressions in the
  correct forms.  
-}
collectExprs :: Compiler ()
collectExprs = undefined


store :: M.Source -> Text -> IO ()
store (M.Module n its) src = runSqlite "hk.db" $ do
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
        
      
    mkBinding (B.Binding m (N.Name _ n)) = 
      MBinding n (B.isRef m) (B.isMut m)
      
    storeBinding = 
      insert . mkBinding
        
    storeFn modId (F.Function oi (N.Name _ n) args t b) = do
      let oidat = toStrict $ encode oi
          tdat  = toStrict $ encode t
          bdat  = toStrict $ encode b
          
      argIds <- insertMany (map mkBinding args)
      insert_ $ MFnItem modId n oidat argIds (Just tdat) (Just bdat)
      
    storeVar modId (V.Variable n t b) = do
      let tdat = toStrict $ encode t
          bdat = toStrict $ encode b
      bindingId <- storeBinding n
      insert_ $ MVarItem modId bindingId (Just tdat) (Just bdat)
    
    storeRec modId (R.Record (N.Name _ n) fs) = do
      recId <- insert $ MRecItem modId n []
      fldIds <- insertMany $ map (mkRecField recId) fs
      update recId [MRecItemFieldIds =. fldIds]
      
    mkRecField recId (R.RecordField (N.Name _ n) t) =
      let tdat = toStrict $ encode t
      in MRecField recId n tdat
      
    mkAlias modId (A.Alias (N.Name _ n) t) =
      let tdat = toStrict $ encode t
      in MAliasItem modId n tdat
    
    storeAlias modId a =
      insert_ $ mkAlias modId a