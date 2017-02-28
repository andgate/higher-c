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
  This phase collects modules and inserts them into a sqlite db on disk
  for fast look-up and sharing with other projects.
-}
collect :: Compiler ()
collect = do
  insertTopLevel
  insertExprs


{-
  Global collection consists of storing only the global information.
  This phase will make symbol information available in the database.
  This phase does not parse expressions, since it doesn't have access
  to enough information to make a symbol table.
-}
insertTopLevel :: Compiler ()
insertTopLevel = do
  xs <- srcFiles <$> St.get
  forM_ xs $ \x -> liftIO $ do
      src <- Text.readFile x
      m <- P.mangledParse src
      insertModule m src

{-
  For each module
    Get list of imports
    
-}

insertModule :: M.Source -> Text -> IO ()
insertModule (M.Module n its) src = runSqlite "hk.db" $ do
  runMigration Db.migrateAll
  modId <- insert $ Db.Module n src [] []
  mapM_ (insertItem modId) its
  
  where
    insertItem modId i =
      case i of
        I.Import n -> return () -- Handled later
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
      
      
    insertExprDecl modId (EDecl.ExprDecl (N.Name n p) oi t) = do
      let oidat = toStrict $ encode oi
          tdat  = toStrict $ encode t
          
      insert $ Db.ExprDecl modId n oidat tdat
      
      
    insertVarOp modId fnId n (OI.OpInfo p a) = do
      opId <- insert $ Db.Op modId n (fromIntegral p) a
      insert_ $ Db.VarOp opId fnId
      
    
    insertTypeDecl modId (TD.TypeDecl ctx (N.Name n p) ts) = do
      let ctxdat = toStrict $ encode ctx
          tsdat  = map (toStrict . encode) ts
          
      insert $ Db.TypeDecl modId n ctxdat tsdat
  
    
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
      conIds <- mapM (insertDataCon modId ddId) b
      update ddId [Db.DataDefBody =. conIds]
      return ddId
      
    insertDataCon modId ddId (DD.DataCons (N.Name n p) b) = do
      conId <- insert $ Db.DataCon modId ddId n []
      memIds <- insertMany (mkDataMem modId ddId conId <$> b)
      update conId [Db.DataConMembers =. memIds]
      return conId

      
    mkDataMem modId ddId conId (DD.Tagged (N.Name n p) t) =
      let tdat = toStrict $ encode t
      in Db.DataMember modId ddId conId (Just n) tdat
      
    mkDataMem modId ddId conId (DD.Tagless t) =
      let tdat = toStrict $ encode t
      in Db.DataMember modId ddId conId Nothing tdat
      
    
    
    insertTypeClassDef_ modId tcd = do
      insertTypeClassDef modId tcd
      return ()
     
    insertTypeClassDef modId (TCD.TypeClassDef d eds) = do
      decId <- insertTypeDecl modId d
      edIds <- mapM (insertExprDef modId) eds
      insert $ Db.TypeClassDef modId decId edIds
      
    
    
{- from older syntax  
    insertRec modId (R.Record (N.Name n p) fs) = do
      recId <- insert $ MRecItem modId n []
      fldIds <- insertMany $ map (mkRecField recId) fs
      update recId [MRecItemFieldIds =. fldIds]
      
    mkRecField recId (R.RecordField (N.Name n p) t) =
      let tdat = toStrict $ encode t
      in MRecField recId n tdat
      
    mkAlias modId (A.Alias (N.Name n p) t) =
      let tdat = toStrict $ encode t
      in MAliasItem modId n tdat
      
    
    insertAlias modId a =
      insert_ $ mkAlias modId a
      
-}
      
      
{-
  Expression collection is run after symbol tables are generated.
  These symbol tables are used to parse expressions in the
  correct forms.  
-}
insertExprs :: Compiler ()
insertExprs = error "Collect expressions not implemented"