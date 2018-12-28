{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Language.HigherC.Analysis.Namecheck where

import Control.Lens
import Control.Monad
import Control.Monad.Writer hiding (Alt)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Set (Set)
import Data.Trie (Trie)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc hiding (group)
import Data.Bitraversable

import Language.HigherC.Syntax.Concrete
import Language.HigherC.Syntax.Location
import Language.HigherC.Analysis.Namecheck.Scope

import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty  as NE
import qualified Data.Trie           as Trie


data NameError
  = NameNotFound Loc Text
  | NameConflict Text Loc Loc


instance Pretty NameError where
    pretty = \case
        NameNotFound l n ->
            pretty l <+> "Name Not Found:" <+> pretty n
        
        NameConflict n l l' ->
            vsep [ "Name conflict detected between"
                 , pretty n <+> "at" <+> pretty l'
                 , "and" <+> pretty n <+> "at" <+> pretty l
                 ]


-- Namechecking and building should be ran over
-- groups of objects, which will be fused
-- into a final build product...
namecheck :: [Object] -> InterfaceTable -> ([Object], [NameError])
namecheck objs interface_table
  = runNamecheck (namecheckObjects objs) initial_scope
  where
    initial_scope = mempty { _globalInterfaceTable = interface_table }


-- Helper types to manage reader/writer stack...
type MonadNameCheck m = (MonadState LocalScope m, MonadReader GlobalScope m, MonadWriter [NameError] m)
type Namecheck a = StateT LocalScope (ReaderT GlobalScope (Writer [NameError])) a

-- Helper function to run reader/writer stack...
runNamecheck :: Namecheck a -> GlobalScope -> (a, [NameError])
runNamecheck m env = runWriter $ runReaderT (evalStateT m mempty) env


-- -----------------------------------------------------------------------------
-- | Object Name Checking

namecheckObjects :: MonadNameCheck m => [Object] -> m [Object]
namecheckObjects objs
  = local (<> objects_scope) $ mapM namecheckObject objs
  where
    objects_scope = mempty { _globalModuleTable = buildModuleTable objs }

namecheckObject :: MonadNameCheck m => Object -> m Object
namecheckObject obj@Object{..} = do
  itable <- view globalInterfaceTable
  let object_scope = objectScope obj
      imports_scope = mconcat (interfaceScope <$> imports_ifaces)
      imports_ifaces = [ fromJust (Map.lookup n itable) | n <- imports ]
      imports = unpackImport <$> findModuleSurfaceImports obj

  objBody' <- local (<> object_scope <> imports_scope)
                $ mapM namecheckModuleStmt objBody
  return $ obj { objBody = objBody' }

-- -----------------------------------------------------------------------------
-- | Module Name Checking

namecheckModule :: MonadNameCheck m => Module -> m Module
namecheckModule m@Module{..} = do
  mtable <- view globalModuleTable
  itable <- view globalInterfaceTable
  let module_scope = moduleScope m
      imports_scope = mconcat $ (moduleScope <$> imports_mods) <> (interfaceScope <$> imports_ifaces)
      imports_mods   = catMaybes [ Map.lookup i mtable | i <- import_paths ]
      imports_ifaces = catMaybes [ Map.lookup i itable | i <- import_paths ]
      import_paths = unpackImport <$> findModuleSurfaceImports m

  modBody' <- local (<> module_scope <> imports_scope)
                $ mapM namecheckModuleStmt modBody
  
  return $ m {modBody = modBody'}


namecheckModuleStmt :: MonadNameCheck m => ModuleStmt -> m ModuleStmt
namecheckModuleStmt = \case
  MModule m -> MModule <$> namecheckModule m

  MImport i -> return $ MImport i

  MVarDefn    var    -> MVarDefn    <$> namecheckVarDefn var
  MFuncDefn   fn     -> MFuncDefn   <$> namecheckFuncDefn fn
  MFuncExtern exfunc -> MFuncExtern <$> namecheckFuncExtern exfunc

  MCtor ctor -> MCtor <$> namecheckCtor ctor
  MDtor dtor -> MDtor <$> namecheckDtor dtor

  MTypeDefn  type_defn  -> MTypeDefn  <$> namecheckTypeDefn  type_defn
  MAliasDefn alias_defn -> MAliasDefn <$> namecheckAliasDefn alias_defn

  MClass c -> MClass <$> namecheckClass c
  MInst  i -> MInst  <$> namecheckInstance i

  MOpDecl opdecl -> return $ MOpDecl opdecl


-- -----------------------------------------------------------------------------
-- | Name Checking Variables

namecheckVarDefn :: MonadNameCheck m => VarDefn -> m VarDefn
namecheckVarDefn (VarDefn l1 (VarDecl l2 n may_typesig) may_exp)
  = vardefn
  where
    vardefn = VarDefn l1 <$> vardecl <*> traverse namecheckExp may_exp
    vardecl = VarDecl l2 n <$> traverse namecheckTypesig may_typesig
  


-- -----------------------------------------------------------------------------
-- | Name Checking Functions

namecheckFuncDefn :: MonadNameCheck m => FuncDefn -> m FuncDefn
namecheckFuncDefn (FuncDefn l1 (FuncDecl l2 specs n may_scheme params may_typesig) block)
  = fndefn
  where
    fndefn = FuncDefn l1 <$> fndecl <*> namecheckBlock block
    fndecl = FuncDecl l2 specs n <$> traverse namecheckScheme may_scheme
                                 <*> namecheckParams params
                                 <*> traverse namecheckTypesig may_typesig


namecheckFuncExtern :: MonadNameCheck m => FuncExtern -> m FuncExtern
namecheckFuncExtern ex = undefined

-- -----------------------------------------------------------------------------
-- | Name Checking Parameters

namecheckParams :: MonadNameCheck m => Parameters -> m Parameters
namecheckParams (Parameters params)
  = Parameters <$> mapM namecheckParam params

namecheckParam :: MonadNameCheck m => Parameter -> m Parameter
namecheckParam (Parameter l n may_typesig)
  = Parameter l n <$> traverse namecheckTypesig may_typesig


-- -----------------------------------------------------------------------------
-- | Name Checking Constructors

namecheckCtor :: MonadNameCheck m => CtorDefn -> m CtorDefn
namecheckCtor ctor = undefined


-- -----------------------------------------------------------------------------
-- | Name Checking Destructors

namecheckDtor :: MonadNameCheck m => DtorDefn -> m DtorDefn
namecheckDtor dtor = undefined

-- -----------------------------------------------------------------------------
-- | Name Checking Type Definitions

namecheckTypeDefn :: MonadNameCheck m => TypeDefn -> m TypeDefn
namecheckTypeDefn defn = undefined


-- -----------------------------------------------------------------------------
-- | Name Checking Type Alias Definitions

namecheckAliasDefn :: MonadNameCheck m => AliasDefn -> m AliasDefn
namecheckAliasDefn alias = undefined

-- -----------------------------------------------------------------------------
-- | Name Checking Type Class Definitions

namecheckClass :: MonadNameCheck m => ClassDefn -> m ClassDefn
namecheckClass c = undefined

-- -----------------------------------------------------------------------------
-- | Name Checking Type Class Instances

namecheckInstance :: MonadNameCheck m => InstDefn -> m InstDefn
namecheckInstance i = undefined

-- -----------------------------------------------------------------------------
-- | Name Checking Type Signatures

namecheckTypesig :: MonadNameCheck m => TypeSig -> m TypeSig
namecheckTypesig (TypeSig l ty) = TypeSig l <$> namecheckType ty

-- -----------------------------------------------------------------------------
-- | Name Checking Statements

namecheckBlock :: MonadNameCheck m => Block -> m Block
namecheckBlock (Block l stmts) =
  Block l <$> mapM namecheckStmt stmts

-- Requires state
namecheckStmt :: MonadNameCheck m => Stmt -> m Stmt
namecheckStmt = \case
  SNop l -> return $ SNop l
  SExp l e -> SExp l <$> namecheckExp e

  SLet l may_specs vdefn -> undefined


  SBlock l block -> SBlock l <$> namecheckBlock block
  SWith l exp stmt -> SWith l <$> namecheckExp exp <*> namecheckStmt stmt

  SBreak l -> return $ SBreak l
  SContinue l -> return  $ SContinue l
  SReturn l may_exp -> SReturn l <$> traverse namecheckExp may_exp
  SThrow l exp -> SThrow l <$> namecheckExp exp

  SIf l cond branch may_else
    -> SIf l <$> namecheckExp cond <*> namecheckStmt branch <*> traverse namecheckStmt may_else

  SWhile l cond loop_body   
    -> SWhile   l <$> namecheckExp  cond      <*> namecheckStmt loop_body
  
  SDoWhile l loop_body cond 
    -> SDoWhile l <$> namecheckStmt loop_body <*> namecheckExp  cond

  SFor l init may_cond may_iter body
    -> SFor l <$> bitraverse (traverse namecheckExp) namecheckVarDefn init
              <*> traverse namecheckExp may_cond
              <*> traverse namecheckExp may_iter
              <*> namecheckStmt body

  SCase l e alts
    -> SCase l <$> namecheckExp e <*> namecheckAlts alts

  STryCatch l try catches may_finally
    -> STryCatch l <$> namecheckTry try <*> mapM namecheckCatch catches <*> traverse namecheckFinally may_finally

namecheckTry :: MonadNameCheck m => Try -> m Try
namecheckTry (Try l stmt) = Try l <$> namecheckStmt stmt

namecheckCatch :: MonadNameCheck m => Catch -> m Catch
namecheckCatch (Catch l ex stmt) = Catch l <$> namecheckExp ex <*> namecheckStmt stmt

namecheckFinally :: MonadNameCheck m => Finally -> m Finally
namecheckFinally (Finally l stmt) = Finally l <$> namecheckStmt stmt


-- -----------------------------------------------------------------------------
-- | Name Checking Patterns

namecheckPat :: MonadNameCheck m => Pat -> m Pat
namecheckPat pat = undefined

-- -----------------------------------------------------------------------------
-- | Name Checking Alternatives

namecheckAlts :: MonadNameCheck m => Alts -> m Alts
namecheckAlts (Alts l alts) = Alts l <$> mapM namecheckAlt alts

namecheckAlt :: MonadNameCheck m => Alt -> m Alt
namecheckAlt (Alt l pat stmt) = Alt l <$> namecheckPat pat <*> namecheckStmt stmt

-- -----------------------------------------------------------------------------
-- | Name Checking Expressions

-- Probably requires state lol
namecheckExp :: MonadNameCheck m => Exp -> m Exp
namecheckExp = \case
  EVar n -> undefined -- does this exist in scope?


-- -----------------------------------------------------------------------------
-- | Name Checking Types


namecheckType :: MonadNameCheck m => Type -> m Type
namecheckType = \case
  TVar n -> undefined

  -- -----------------------------------------------------------------------------
-- | Name Checking Type Schemes

-- Needs to update local type state >_>
namecheckScheme :: MonadNameCheck m => Scheme -> m Scheme
namecheckScheme (Scheme l preds)
  = Scheme l <$> (traverse namecheckPred preds)

namecheckPred :: MonadNameCheck m => Pred -> m Pred
namecheckPred = \case
  Forall l n -> return $ Forall l n -- This needs to be added to local state
  IsIn _ n tys -> undefined -- add tys to local state lol

-- -----------------------------------------------------------------------------
-- | Name Checking Kinds

-- -----------------------------------------------------------------------------
-- | Name Checking Helpers

inScope :: MonadNameCheck m => Text -> m Bool
inScope name =
  (||) <$> inLocalScope name <*> inGlobalScope name

inLocalScope :: MonadNameCheck m => Text -> m Bool
inLocalScope name = do
  stack <- use localStack
  return undefined


inGlobalScope :: MonadNameCheck m => Text -> m Bool
inGlobalScope name = undefined