{-# LANGUAGE RankNTypes
           , OverloadedStrings
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , InstanceSigs
  #-}
module Language.Hawk.Cache.Item where

import Control.Monad (void, when, forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Binary
import Data.Text (Text, pack, intercalate)
import Database.Persist
import Language.Hawk.Cache.Model (BackendT)
import Language.Hawk.Report.Result
import Language.Hawk.DepExpansion

import qualified Data.ByteString.Lazy as BS
import qualified Language.Hawk.Cache.Model as Db
import qualified Language.Hawk.Cache.Types as Db
import qualified Language.Hawk.Syntax as Syn

class Cacheable a where
    cache :: MonadIO m => a -> BackendT m (Result ())


instance (Syn.BinaryX x) => Cacheable ( Db.PackageId
                                  , Db.ModuleId
                                  , Db.ModuleFileId
                                  , Syn.Item x
                                  ) where
    cache (pid, mid, mfid, i) = do
      iid <- insert $ Db.Item pid mid mfid Db.Fresh (BS.toStrict $ encode i)

      case i of
        Syn.DepItem dep -> void $ cache (iid, dep)
        Syn.ForeignItem _ -> insert_ $ Db.Foreign iid
        Syn.ExposeItem _ -> insert_ $ Db.Expose iid

        Syn.VowItem _ -> insert_ $ Db.Vow iid
        Syn.SigItem _ -> insert_ $ Db.TypeSig iid

        Syn.VarItem _ -> insert_ $ Db.Var iid
        Syn.ValItem _ -> insert_ $ Db.Val iid
        Syn.FunItem _ -> insert_ $ Db.Fun iid
        
        Syn.NewTyItem _ -> insert_ $ Db.NewType iid
        Syn.TyAliasItem _ -> insert_ $ Db.TypeAlias iid
        
        Syn.TyClassItem _ -> insert_ $ Db.TypeClass iid
        Syn.TyInstItem _ -> insert_ $ Db.TypeClassInst iid

        Syn.DataItem _ -> insert_ $ Db.DataType iid

      return $ return ()


instance Cacheable (Db.ItemId, Syn.Dependency) where
    cache :: MonadIO m => (Db.ItemId, Syn.Dependency) -> BackendT m (Result ())
    cache (iid, dep) = do
        forM_ (expandDep dep) $ \(EDep ps ex q a) ->
            let p = intercalate "." ps
            in insert_ $ Db.ExpandedDependency iid p ex q a
        return $ return ()

{-
insertItem :: MonadIO m => Db.ModuleId -> I.Source -> BackendT m ()
insertItem modId i =
  case i of
    I.DepDecl d -> void $ insertDepDecl modId d
    I.ExprDef ed -> void $ insertExprDef modId ed
    I.AliasDef ad -> void $ insertAliasDef modId ad
    I.DataDef dd -> void $ insertDataDef modId dd
    I.TypeClassDef tcd -> void $ insertTypeClassDef modId tcd


insertDepDecl :: MonadIO m => Db.ModuleId -> I.Dependency -> BackendT m Db.DependencyId
insertDepDecl modId (I.Dep q p a) = do
  let pdat = toStrict $ encode p
  insert $ Db.Dependency modId q pdat a


insertExprDef :: MonadIO m => Db.ModuleId -> EDef.Source -> BackendT m Db.ExprDefId
insertExprDef modId (EDef.ExprDef d e) = do
  declId <- insertExprDecl modId d
  let edat  = toStrict $ encode e
  insert $ Db.ExprDef modId declId edat

insertExprDecl :: MonadIO m => Db.ModuleId -> EDecl.Source -> BackendT m Db.ExprDeclId
insertExprDecl modId (EDecl.ExprDecl (N.Name n p) oi vs t) = do
  opId <- insertOperator modId n oi
  let tdat  = toStrict $ encode t
  insert $ Db.ExprDecl modId n opId tdat
  
insertTypeDecl :: MonadIO m => Db.ModuleId -> TD.Source -> BackendT m Db.TypeDeclId
insertTypeDecl modId (TD.TypeDecl ctx (N.Name n p) oi ts) = do
  opId <- insertOperator modId n oi 
  let ctxdat = toStrict $ encode ctx
      tsdat  = map (toStrict . encode) ts
  insert $ Db.TypeDecl modId n opId ctxdat tsdat
  
insertOperator :: MonadIO m => Db.ModuleId -> Text -> OI.OpInfo -> BackendT m Db.OpId
insertOperator modId n (OI.OpInfo p a) =
  insert $ Db.Op modId n (fromIntegral p) a

insertAliasDef :: MonadIO m => Db.ModuleId -> AD.Source -> BackendT m Db.AliasDefId  
insertAliasDef modId (AD.AliasDef d t) = do
  declId <- insertTypeDecl modId d
  let tdat  = toStrict $ encode t
  insert $ Db.AliasDef modId declId tdat

insertDataDef :: MonadIO m => Db.ModuleId -> DD.Source -> BackendT m Db.DataDefId
insertDataDef modId (DD.DataDef d b) = do
  declId <- insertTypeDecl modId d
  -- Create an entry for the data def
  ddId <- insert $ Db.DataDef modId declId []
  
  -- insert the constructors and update the data def entry
  consIds <- mapM (insertDataCons modId ddId) b
  update ddId [Db.DataDefBody =. consIds]
  return ddId

insertDataCons :: MonadIO m => Db.ModuleId -> Db.DataDefId -> DD.DCSource -> BackendT m Db.DataConId 
insertDataCons modId ddId (DD.DataCons (N.Name n p) t b) = do
  let tdat = toStrict $ encode t 
  conId <- insert $ Db.DataCon modId ddId n tdat []
  rowIds <- insertMany (mkDataRow modId ddId conId <$> b)
  update conId [Db.DataConRows =. rowIds]
  return conId

mkDataRow :: Db.ModuleId -> Db.DataDefId -> Db.DataConId -> DD.DRSource -> Db.DataRow 
mkDataRow modId ddId conId (DD.DataRow mn t) =
  let tdat = toStrict $ encode t
      mtxt = N.exLocal <$> mn
  in Db.DataRow modId ddId conId mtxt tdat
  
insertTypeClassDef :: MonadIO m => Db.ModuleId -> TCD.Source -> BackendT m Db.TypeClassDefId  
insertTypeClassDef modId (TCD.TypeClassDef d eds) = do
  decId <- insertTypeDecl modId d
  edIds <- mapM (insertExprDef modId) eds
  insert $ Db.TypeClassDef modId decId edIds

-}