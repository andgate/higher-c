{-# LANGUAGE TypeSynonymInstances, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell,TypeFamilies #-}
module Language.Hawk.Metadata where


import Data.Binary (encode, decode)

import Control.Monad (forM_)
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


import qualified Data.Text.Lazy.IO as Text
import qualified Control.Monad.Trans.State.Strict as St
import qualified Language.Hawk.Parse as P
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
    assoc F.Assoc
    
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
  collectExprs


{-
  Global collection consists of storing only the global information.
  This phase will make symbol information available in the database.
  This phase does not parse expressions, since it doesn't have access
  to enough information to make a symbol table.
-}
collectGlobal :: Compiler ()
collectGlobal = do
  xs <- srcFiles <$> St.get
  forM_ xs $ \x -> liftIO $ do
      src <- Text.readFile x
      m <- P.mangledParse src
      store m src

{-
  Expression collection is run after symbol tables are generated.
  These symbol tables are used to parse expressions in the
  correct forms.  
-}
collectExprs :: Compiler ()
collectExprs = error "Collect expressions not implemented"


{-
  For each module
    Get list of imports
    
-}

store :: M.Source -> Text -> IO ()
store (M.Module n its) src = runSqlite "hk.db" $ do
  runMigration migrateAll
  modId <- insert $ MModule n src (I.getDeps its) []
  mapM_ (storeItem modId) its
  
  where
    storeItem modId i =
      case i of
        I.Import n -> return () -- Handled later
        I.Export n -> return () -- Handled later
        I.Function f -> storeFn modId f
        I.Variable v -> storeVar modId v
        I.Record r -> storeRec modId r
        I.Alias a -> storeAlias modId a
        
      
    mkBinding (B.Binding m (N.Name n p)) = 
      MBinding n (B.isRef m) (B.isMut m)
      
    storeBinding = 
      insert . mkBinding
        
    storeFn modId (F.Function oi (N.Name n p) args t b) = do
      let oidat = toStrict $ encode oi
          tdat  = toStrict $ encode t
          bdat  = toStrict $ encode b
          
      argIds <- insertMany (map mkBinding args)
      fnId <- insert $ MFnItem modId n oidat argIds (Just tdat) (Just bdat)
      
      storeVarOp modId fnId n oi
      
      
    storeVarOp modId fnId n (F.OpInfo p a) = do
      opId <- insert $ MOp modId n (fromIntegral p) a
      insert_ $ MVarOp opId fnId
      
      
    storeVar modId (V.Variable n t b) = do
      let tdat = toStrict $ encode t
          bdat = toStrict $ encode b
      bindingId <- storeBinding n
      insert_ $ MVarItem modId bindingId (Just tdat) (Just bdat)
    
    storeRec modId (R.Record (N.Name n p) fs) = do
      recId <- insert $ MRecItem modId n []
      fldIds <- insertMany $ map (mkRecField recId) fs
      update recId [MRecItemFieldIds =. fldIds]
      
    mkRecField recId (R.RecordField (N.Name n p) t) =
      let tdat = toStrict $ encode t
      in MRecField recId n tdat
      
    mkAlias modId (A.Alias (N.Name n p) t) =
      let tdat = toStrict $ encode t
      in MAliasItem modId n tdat
      
    
    storeAlias modId a =
      insert_ $ mkAlias modId a