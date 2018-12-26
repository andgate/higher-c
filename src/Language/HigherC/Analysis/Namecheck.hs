{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Language.HigherC.Analysis.Namecheck where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Set (Set)
import Data.Trie (Trie)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc hiding (group)

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



namecheck :: Object -> InterfaceMap -> [NameError]
namecheck obj import_dict
  = runNamecheck (namecheckObject obj) initial_scope
  where
    initial_scope = mempty { scopeIfaces = import_dict }


-- Helper types to manage reader/writer stack...
type MonadNameCheck m = (MonadReader Scope m, MonadWriter [NameError] m)
type Namecheck a = ReaderT Scope (Writer [NameError]) a

-- Helper function to run reader/writer stack...
runNamecheck :: Namecheck a -> Scope -> [NameError]
runNamecheck m env = execWriter $ runReaderT m env


-- -----------------------------------------------------------------------------
-- | Object Name Checking

namecheckObject :: MonadNameCheck m => Object -> m ()
namecheckObject obj@Object{..} = do
  ifaces <- scopeIfaces <$> ask
  let object_scope = objectScope obj
      imports_scope = mconcat (interfaceScope <$> imports_ifaces)
      imports_ifaces = [ fromJust (Map.lookup n ifaces) | n <- imports ]
      imports = unpackImport <$> findModuleSurfaceImports obj
  local (<> object_scope <> imports_scope)
    $ mapM_ namecheckModuleStmt objBody

-- -----------------------------------------------------------------------------
-- | Module Name Checking

namecheckModule :: MonadNameCheck m => Module -> m ()
namecheckModule m@Module{..} = do
  ifaces <- scopeIfaces <$> ask
  let module_scope = moduleScope m
      imports_scope = mconcat (interfaceScope <$> imports_ifaces)
      imports_ifaces = [ fromJust (Map.lookup n ifaces) | n <- imports ]
      imports = unpackImport <$> findModuleSurfaceImports m
  local (<> module_scope <> imports_scope)
    $ mapM_ namecheckModuleStmt modBody


namecheckModuleStmt :: MonadNameCheck m => ModuleStmt -> m ()
namecheckModuleStmt = \case
  MModule m -> namecheckModule m

  MImport i -> return ()

  MVarDefn    var    -> namecheckVarDefn var
  MFuncDefn   fn     -> namecheckFuncDefn fn
  MFuncExtern exfunc -> namecheckFuncExtern exfunc

  MCtor ctor -> namecheckCtor ctor
  MDtor dtor -> namecheckDtor dtor

  MTypeDefn  type_defn  -> namecheckTypeDefn  type_defn
  MAliasDefn alias_defn -> namecheckAliasDefn alias_defn

  MClass c -> namecheckClass c
  MInst  i -> namecheckInstance i

  MOpDecl _ -> return ()


-- -----------------------------------------------------------------------------
-- | Name Checking Variables

namecheckVarDefn :: MonadNameCheck m => VarDefn -> m ()
namecheckVarDefn (VarDefn _ (VarDecl _ _ may_typesig) may_exp) = void $ do
  traverse namecheckTypesig may_typesig
  traverse namecheckExp may_exp


-- -----------------------------------------------------------------------------
-- | Name Checking Functions

namecheckFuncDefn :: MonadNameCheck m => FuncDefn -> m ()
namecheckFuncDefn (FuncDefn _ (FuncDecl _ _ _ may_scheme params may_typesig) block) = void $ do
  traverse namecheckScheme may_scheme
  namecheckParams params
  traverse namecheckTypesig may_typesig
  namecheckBlock block

namecheckFuncExtern :: MonadNameCheck m => FuncExtern -> m ()
namecheckFuncExtern ex = undefined

namecheckCtor :: MonadNameCheck m => CtorDefn -> m ()
namecheckCtor ctor = undefined

namecheckDtor :: MonadNameCheck m => DtorDefn -> m ()
namecheckDtor dtor = undefined

namecheckTypeDefn :: MonadNameCheck m => TypeDefn -> m ()
namecheckTypeDefn defn = undefined

namecheckAliasDefn :: MonadNameCheck m => AliasDefn -> m ()
namecheckAliasDefn alias = undefined

namecheckClass :: MonadNameCheck m => ClassDefn -> m ()
namecheckClass c = undefined

namecheckInstance :: MonadNameCheck m => InstDefn -> m ()
namecheckInstance i = undefined

-- -----------------------------------------------------------------------------
-- | Name Checking Type Signatures

namecheckTypesig :: MonadNameCheck m => TypeSig -> m ()
namecheckTypesig (TypeSig _ ty) = namecheckType ty

-- -----------------------------------------------------------------------------
-- | Name Checking Statements

namecheckBlock :: MonadNameCheck m => Block -> m ()
namecheckBlock (Block _ stmts) = undefined

-- -----------------------------------------------------------------------------
-- | Name Checking Expressions

namecheckExp :: MonadNameCheck m => Exp -> m ()
namecheckExp = \case
  EVar n -> undefined


-- -----------------------------------------------------------------------------
-- | Name Checking Types

namecheckType :: MonadNameCheck m => Type -> m ()
namecheckType = \case
  TVar n -> undefined

  -- -----------------------------------------------------------------------------
-- | Name Checking Type Schemes

namecheckScheme :: MonadNameCheck m => Scheme -> m ()
namecheckScheme (Scheme _ preds) = undefined

namecheckPred :: MonadNameCheck m => Pred -> m ()
namecheckPred = \case
  Forall _ _ -> return ()
  IsIn _ n tys -> undefined