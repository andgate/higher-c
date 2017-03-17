{-# LANGUAGE TypeSynonymInstances, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell,TypeFamilies #-}
module Language.Hawk.Metadata where


import Data.Binary (encode, decode)

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
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

import qualified Language.Hawk.Metadata.Namespace as NS
import qualified Language.Hawk.Metadata.Schema as Db
import qualified Language.Hawk.Parse as P
import qualified Language.Hawk.Syntax.AliasDefinition as AD
import qualified Language.Hawk.Syntax.DataDefinition as DD
import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.ExpressionDeclaration as EDecl
import qualified Language.Hawk.Syntax.ExpressionDefinition as EDef
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Literal as L
import qualified Language.Hawk.Syntax.Module as M
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.OpInfo as OI
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeClassDefinition as TCD
import qualified Language.Hawk.Syntax.TypeDeclaration as TD


{-
  Builds a collection of Module.Source in a sqlite db from the compiler state.
-}
-- TODO: History-aware collection
collect :: Compiler ()
collect = do
  s <- St.get
  -- Insert all the given packages
  mapM_ (liftIO . insertPackage) . packages
  -- Build the module namespaces
  NS.build
  

insertPackage :: Package -> IO ()
insertPackage pkg@(Package n xs) = do
  pkgId <- createPackage pkg
  forM_ xs $ \x -> do
      src <- Text.readFile x
      m <- P.parseTopLevel src
      insertModule pkgId m src
      
      -- For testing only
      P.parseTest src


createPackage :: Package -> IO Db.PackageId
createPackage (Package n _) = runSqlite "hk.db" $ do
  runMigration Db.migrateAll
  insert $ Db.Package n
  

insertModule :: Db.PackageId -> M.Source -> Text -> IO ()
insertModule pkgId (M.Module n its) src = runSqlite "hk.db" $ do
  runMigration Db.migrateAll
  modId <- insert $ Db.Module pkgId n src
  mapM_ (insertItem modId) its
  
  where
    insertItem modId i =
      case i of
        I.Import n _ -> return () -- Handled later
        I.Export n -> return () -- Handled later
        I.ExprDef ed -> insertExprDef_ modId ed
        I.AliasDef ad -> insertAliasDef_ modId ad
        I.DataDef dd -> insertDataDef_ modId dd
        I.TypeClassDef tcd -> insertTypeClassDef_ modId tcd
    
    
    insertExprDef_ modId ed = do
      insertExprDef modId ed
      return ()
    
    insertExprDef modId (EDef.ExprDef d e) = do
      let edat  = toStrict $ encode e
      decId <- insertExprDecl modId d
          
      insert $ Db.ExprDef modId decId edat
      
    
    insertExprDecl modId (EDecl.ExprDecl (N.Name n p) oi vs t) = do
      let tdat  = toStrict $ encode t
      opId <- insertOperator modId n oi
      insert $ Db.ExprDecl modId n opId tdat
      
    
    insertTypeDecl modId (TD.TypeDecl ctx (N.Name n p) oi ts) = do
      let ctxdat = toStrict $ encode ctx
          tsdat  = map (toStrict . encode) ts
      opId <- insertOperator modId n oi    
      insert $ Db.TypeDecl modId n opId ctxdat tsdat
      
      
    insertOperator modId n (OI.OpInfo p a) = do
      insert $ Db.Op modId n (fromIntegral p) a
  
    
    insertAliasDef_ modId ad = do
      insertAliasDef modId ad
      return ()
     
    insertAliasDef modId (AD.AliasDef d t) = do
      let tdat  = toStrict $ encode t
      declId <- insertTypeDecl modId d 
      insert $ Db.AliasDef modId declId tdat
      
    
    insertDataDef_ modId dd = do
      insertDataDef modId dd
      return ()
    
    insertDataDef modId (DD.DataDef d b) = do
      -- Create an entry for the data def
      declId <- insertTypeDecl modId d
      ddId <- insert $ Db.DataDef modId declId []
      
      -- insert the constructors and update the data def entry
      consIds <- mapM (insertDataCons modId ddId) b
      update ddId [Db.DataDefBody =. consIds]
      return ddId
      
    insertDataCons modId ddId (DD.DataCons (N.Name n p) t b) = do
      let tdat = toStrict $ encode t 
      conId <- insert $ Db.DataCon modId ddId n tdat []
      rowIds <- insertMany (mkDataRow modId ddId conId <$> b)
      update conId [Db.DataConRows =. rowIds]
      return conId

    mkDataRow modId ddId conId (DD.DataRow mn t) =
      let tdat = toStrict $ encode t
          mtxt = N.exLocal <$> mn
      in Db.DataRow modId ddId conId mtxt tdat
      
    
    
    insertTypeClassDef_ modId tcd = do
      insertTypeClassDef modId tcd
      return ()
     
    insertTypeClassDef modId (TCD.TypeClassDef d eds) = do
      decId <- insertTypeDecl modId d
      edIds <- mapM (insertExprDef modId) eds
      insert $ Db.TypeClassDef modId decId edIds