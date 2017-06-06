module Language.Hawk.Cache.Item where

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