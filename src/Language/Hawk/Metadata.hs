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
import qualified Language.Hawk.Syntax.ClassDefinition as CD
import qualified Language.Hawk.Syntax.ClassInstance as CI
import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.Item as I
import qualified Language.Hawk.Syntax.Literal as L
import qualified Language.Hawk.Syntax.Module as M
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.OpInfo as OI
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Record as R
import qualified Language.Hawk.Syntax.TaggedUnion as TU
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDefinition as TD


{-
  This phase collects modules and stores them into a sqlite db on disk
  for fast look-up and sharing with other projects.
-}
collect :: Compiler ()
collect = do
  storeTopLevel
  storeExprs


{-
  Global collection consists of storing only the global information.
  This phase will make symbol information available in the database.
  This phase does not parse expressions, since it doesn't have access
  to enough information to make a symbol table.
-}
storeTopLevel :: Compiler ()
storeTopLevel = do
  xs <- srcFiles <$> St.get
  forM_ xs $ \x -> liftIO $ do
      src <- Text.readFile x
      m <- P.mangledParse src
      storeTopLevelFromModule m src

{-
  For each module
    Get list of imports
    
-}

storeModule :: M.Source -> Text -> IO ()
storeModule (M.Module n its) src = runSqlite "hk.db" $ do
  runMigration migrateAll
  modId <- insert $ MModule n src (I.getDeps its) []
  mapM_ (storeItem modId) its
  
  where
    storeItem modId i =
      case i of
        I.Import n -> return () -- Handled later
        I.Export n -> return () -- Handled later
        I.ExprDef ed -> storeED modId ed
        I.TypeDef td -> storeTD modId td
        I.Record r -> storeR modId r
        I.TaggedUnion tu -> storeTU modId tu
        I.ClassDef cs -> storeCD modId cs
        I.ClassInst ci -> storeCI modId ci
        
        
    storeED modId (ED.ExprDef oi (N.Name n p) t b) = do
      let oidat = toStrict $ encode oi
          tdat  = toStrict $ encode t
          bdat  = toStrict $ encode b
          
      fnId <- insert $ Db.ExprDef modId n oidat (Just tdat) (Just bdat)
      
      storeVarOp modId fnId n oi
      
      
    storeVarOp modId fnId n (F.OpInfo p a) = do
      opId <- insert $ MOp modId n (fromIntegral p) a
      insert_ $ MVarOp opId fnId
      
    storeED modId (ED.ExprDef oi (N.Name n p) vs t b) = do
      let oidat = toStrict $ encode oi
          tdat  = toStrict $ encode t
          bdat  = toStrict $ encode b
          
      argIds <- insertMany (map mkBinding args)
      fnId <- insert $ MFnItem modId n oidat argIds (Just tdat) (Just bdat)
    
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
      
      
{-
  Expression collection is run after symbol tables are generated.
  These symbol tables are used to parse expressions in the
  correct forms.  
-}
storeExprs :: Compiler ()
storeExprs = error "Collect expressions not implemented"