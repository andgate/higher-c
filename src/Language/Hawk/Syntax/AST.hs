{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Language.Hawk.Syntax.AST where

import Language.Hawk.Data.Node

import Control.Lens
import Data.Int
import Data.Word
import Data.List
import Data.Generics
import Data.Monoid


class HkAnnotated t where
  annot :: t a -> a
  
instance (HkNode a, HkAnnotated t) => HkNode (t a) where
  nodeInfo = nodeInfo . annot
  
-- ----------------------------------------------------------------------------- 
-- Hawk Module
--
-- | Hawk module declaration
type HkModNode = HkMod NodeInfo
data HkMod a
  = HkMod (HkModPath a) (HkModBlock a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkMod where
  annot (HkMod _ _ a) = a

-- | Hawk item declaration
type HkModItemNode = HkModItem NodeInfo  
data HkModItem a
  = HkModItem (HkVisibilityTag a) (HkItem a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkModItem where
  annot (HkModItem _ _ a) = a

-- | Hawk module block declaration
type HkModBlockNode = HkModBlock NodeInfo
data HkModBlock a
  = HkModBlock [HkModItem a] a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkModBlock where
  annot (HkModBlock _ a) = a
  
-- -----------------------------------------------------------------------------
-- Hawk Items
--
-- | Also known as top-level statements.
type HkItemNode = HkItem NodeInfo
data HkItem a
  -- Module definition statement, defaults to public.
  = HkItemMod         (HkMod a)

  -- Import declaration, may be qualified or unqualified
  -- By default, an import declaration is private.
  | HkItemImport      (HkImportPath a) a
  | HkItemImportQual  (HkImportPath a) a
  
  -- Top-level function statements
  -- At the top-level, a function can be linked, declared, or defined.
  -- A Function at the top-level also requires a visibility tag.
  | HkItemFnLink      (HkFn a) a
  | HkItemFn          (HkFn a)
  -- Top-level binding statement
  | HkItemBind        (HkBind a)
  
  | HkItemType        (HkTypeDef a)
    
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkItem where
  annot (HkItemMod n) = annot n
  annot (HkItemImport _ a) = a
  annot (HkItemImportQual _ a) = a
  
  annot (HkItemFnLink _ a) = a
  annot (HkItemFn n) = annot n
  annot (HkItemBind n) = annot n
  
  annot (HkItemType n) = annot n
  
-- -----------------------------------------------------------------------------
-- | Hawk Visibility Tag
--
-- A visibility tag is used to declare the visibility of a function, variable, or record.
type HkVisibilityTagNode = HkVisibilityTag NodeInfo
data HkVisibilityTag a
  = HkPublic a
  | HkPrivate a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkVisibilityTag where
  annot (HkPublic a) = a
  annot (HkPrivate a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Import
--
-- Import items in hawk are a list of dotted identifiers to some module or
-- external function, record, or variable.
-- 
-- Import items are provided an ident that can overrides their current qualified alias.
-- This can be useful for shorthanded qualified names. An empty alias override will be ignored.
type HkImportPathNode = HkImportPath NodeInfo
data HkImportPath a
  = HkImportTarget (HkImportTarget a) (Maybe (HkName a)) a
  | HkImportPath (HkName a) (HkImportPath a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkImportPath where
  annot (HkImportTarget _ _ a) = a
  annot (HkImportPath _ _ a) = a
  
type HkImportTargetNode = HkImportTarget NodeInfo
data HkImportTarget a
  = HkITarget (HkName a)        -- ^ Object, Function, Type or Module name
  | HkIPaths [HkImportPath a] a   -- ^ A set of import paths.
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkImportTarget where
  annot (HkITarget n) = annot n
  annot (HkIPaths _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Type Context and Class Assertion

-- | A type qualified with a context.
-- An unqualified type has an empty context
type HkQualTypeNode = HkQualType NodeInfo
data HkQualType a
  = HkQualType (HkTypeContext a) (HkType a) a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkQualType where
  annot (HkQualType _ _ a) = a
  
evalQType :: HkQualType a -> HkType a
evalQType (HkQualType _ ty _) = evalType ty

qtypeArgs :: HkQualType a -> [HkType a]
qtypeArgs (HkQualType _ ty _) = typeArgs ty

-- | Type context
type HkTypeContextNode = HkTypeContext NodeInfo
type HkTypeContext a = [HkClassAsst a]

-- | Class assertions
type HkClassAsstNode = HkClassAsst NodeInfo
data HkClassAsst a
  = HkClassAsst (HkQName a) [HkType a] a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkClassAsst where
  annot (HkClassAsst _ _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Type
--
-- A type in Hawk can be primitive type, reference types, a record type, or even a type signature.
type HkTypeNode = HkType NodeInfo
data HkType a
  = HkTyFun         (HkType a) (HkType a) a
  | HkTyApp         (HkType a) (HkType a) a
  
  | HkTyPrim        (HkPrimType a) a
  | HkTyCon         (HkQName a) a
  | HkTyVar         (HkName a) a
  
  | HkTyPtr         (HkType a) a
  | HkTyArray       (HkType a) a
  | HkTyTuple       [HkType a] a
  deriving (Eq, Ord, Show, Data, Typeable)

evalType :: HkType a -> HkType a
evalType (HkTyFun _ t2 _) = evalType t2
evalType t = t

typeArgs :: HkType a -> [HkType a]
typeArgs ty = typeArgs' ty []
  where typeArgs' (HkTyFun t1 t2 _) tys = typeArgs' t2 (tys ++ [t1])
        typeArgs' _ tys = tys -- disregard resulting type
  
instance HkAnnotated HkType where
  annot (HkTyFun _ _ a) = a
  annot (HkTyApp _ _ a) = a
  
  annot (HkTyPrim _ a) = a
  annot (HkTyCon _ a) = a
  annot (HkTyVar _ a) = a
  
  annot (HkTyPtr _ a) = a
  annot (HkTyArray _ a) = a
  annot (HkTyTuple _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Primitive Type
--
-- The primitive types in Hawk. 
type HkPrimTypeNode = HkPrimType NodeInfo
data HkPrimType a
  = HkTyUnit a
  | HkTyBool a
  | HkTyW8 a
  | HkTyW16 a
  | HkTyW32 a
  | HkTyW64 a
  | HkTyI8 a
  | HkTyI16 a
  | HkTyI32 a
  | HkTyI64 a
  | HkTyF32 a
  | HkTyF64 a
  | HkTyChar a
  | HkTyString a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkPrimType where
  annot (HkTyUnit a) = a
  annot (HkTyBool a) = a
  annot (HkTyW8 a) = a
  annot (HkTyW16 a) = a
  annot (HkTyW32 a) = a
  annot (HkTyW64 a) = a
  annot (HkTyI8 a) = a
  annot (HkTyI16 a) = a
  annot (HkTyI32 a) = a
  annot (HkTyI64 a) = a
  annot (HkTyF32 a) = a
  annot (HkTyF64 a) = a
  annot (HkTyChar a) = a
  annot (HkTyString a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Class Constructor
--
-- A class constructor has a dotted identifier for a name, and constructs at least type variable.
--
-- Class constructors are used by @HkTypeContext@ to give a type variable context, restricting those
-- variables to specific interfaces.
type HkClassConsNode = HkClassCons NodeInfo
data HkClassCons a
  = HkClassCons
  { class_cons_name     :: HkQName a
  , class_cons_tyvars   :: [HkName a]
  , class_cons_annot    :: a
  }
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkClassCons where
  annot (HkClassCons _ _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Function Declaration
--
-- A function declaration is the header of a function, containing the name, args, and type signature of a function
type HkFnNode = HkFn NodeInfo
data HkFn a
  = HkFnDec
    { fn_dec_name     :: HkName a
    , fn_dec_type     :: HkQualType a
    , fn_dec_annot    :: a
    }
  | HkFnDef
    { fn_def_name     :: HkName a
    , fn_def_type     :: Maybe (HkQualType a)
    , fn_def_matches  :: [HkMultiMatch a]
    , fn_def_annot    :: a
    }
    deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkFn where
  annot (HkFnDec _ _ a) = a
  annot (HkFnDef _ _ _ a) = a
  
-- -----------------------------------------------------------------------------
-- | Hawk Binding Declaration
--
-- An binding declaration consists of a pattern and a type.
-- The pattern will bind to the default values of the given type.
type HkBindNode = HkBind NodeInfo
data HkBind a
  = HkBindDec
  { bind_dec_name    :: HkPattern a
  , bind_dec_type    :: HkType a
  , bind_dec_annot   :: a
  }
  | HkBindDef
  { bind_def_name    :: HkPattern a
  , bind_def_type    :: Maybe (HkType a)
  , bind_def_exp     :: HkExp a
  , bind_def_annot   :: a
  }
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkBind where
  annot (HkBindDec _ _ a) = a
  annot (HkBindDef _ _ _ a) = a
  
-- -----------------------------------------------------------------------------
-- | Hawk Match
--
-- This binds a patterns to a (possibly) guarded block or expression
type HkMatchNode = HkMatch NodeInfo
data HkMatch a
  = HkMatch
  { match_pat     :: HkPattern a
  , match_block   :: HkMatchRhs a
  , match_annot   :: a
  }
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkMatch where
  annot (HkMatch _ _ a) = a

-- | Multi-match binds a list of patterns to a (possibly) guarded block or expression
type HkMultiMatchNode = HkMultiMatch NodeInfo
data HkMultiMatch a
  = HkMultiMatch
  { mul_match_pats    :: [HkPattern a]
  , mul_match_rhs     :: HkMatchRhs a
  , mul_match_annot   :: a
  }
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkMultiMatch where
  annot (HkMultiMatch _ _ a) = a

-- | Right-hand side of a match
type HkMatchRhsNode = HkMatchRhs NodeInfo
data HkMatchRhs a
  = HkMatchBlock  (HkBlock a) a
  | HkMatchExp    (HkExp a) a
  | HkMatchGuardedRhs [HkRhsGuard a] a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkMatchRhs where
  annot (HkMatchBlock _ a) = a
  annot (HkMatchExp _ a) = a
  annot (HkMatchGuardedRhs _ a) = a 

-- -----------------------------------------------------------------------------
-- | Hawk Patterns
--
-- Patterns are used to match data based on it's structure.
type HkPatternNode = HkPattern NodeInfo
data HkPattern a
  = HkPatIdent  (HkBindingMode a) (HkName a) a
  | HkPatAlias  (HkBindingMode a) (HkName a) (HkPattern a) a
  | HkPatConst  (HkConst a) a
  | HkPatRec    (HkQName a) [HkPattern a] a
  | HkPatTuple  [HkPattern a] a
  | HkPatRef    (HkPattern a) (HkMutability a) a
  | HkPatAny    a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkPattern where
  annot (HkPatIdent _ _ a) = a
  annot (HkPatAlias _ _ _ a) = a
  annot (HkPatConst _ a) = a
  annot (HkPatRec _ _ a) = a
  annot (HkPatTuple _ a) = a
  annot (HkPatRef _ _ a) = a
  annot (HkPatAny a) = a 

-- | Pattern binding mode  
type HkBindingModeNode = HkBindingMode NodeInfo
data HkBindingMode a
  = HkByRef (HkMutability a) a
  | HkByVal (HkMutability a)
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkBindingMode where
  annot (HkByRef _ a) = a
  annot (HkByVal n) = annot n
  
-- | Binding mutability
type HkMutabilityNode = HkMutability NodeInfo
data HkMutability a
  = HkMutable a
  | HkImmutable a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkMutability where
  annot (HkMutable a) = a
  annot (HkImmutable a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Guards
--
-- Guards are a boolean expression that sits in front of a block of code.
type HkGuardNode = HkGuard NodeInfo
data HkGuard a
  = HkGuardExp (HkExp a) a
  | HkGuardAny a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkGuard where
  annot (HkGuardExp _ a) = a
  annot (HkGuardAny a) = a 

-- | Guarded right-hand side of a match
type HkRhsGuardNode = HkRhsGuard NodeInfo
data HkRhsGuard a
  = HkGuardedBlock (HkGuard a) (HkBlock a) a
  | HkGuardedExp   (HkGuard a) (HkExp a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkRhsGuard where
  annot (HkGuardedBlock _ _ a) = a
  annot (HkGuardedExp _ _ a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Type Definition
--
-- Types in hawk are either records, unions or type aliases.
type HkTypeDefNode = HkTypeDef NodeInfo
data HkTypeDef a
  = HkTyAliasDef      (HkTypeAliasDef a)
  | HkTyRecDef        (HkRecDef a)
  | HkTyDataDef       (HkDataDef a)
  | HkTyClassDef      (HkClassDef a)
  | HkTyClassInstDef  (HkClassInstDef a)
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkTypeDef where
  annot (HkTyAliasDef n) = annot n
  annot (HkTyRecDef n) = annot n
  annot (HkTyDataDef n) = annot n
  annot (HkTyClassDef n) = annot n
  annot (HkTyClassInstDef n) = annot n

-- -----------------------------------------------------------------------------  
-- | Hawk Type Alias Definition
--
-- A type alias in Hawk is a new type, that is defined based on another type.
type HkTypeAliasDefNode = HkTypeAliasDef NodeInfo
data HkTypeAliasDef a
  = HkTypeAliasDef
    { alias_name    :: HkName a
    , alias_tyvars  :: [HkName a]
    , alias_context :: HkTypeContext a
    , alias_type    :: HkType a
    , alias_annot   :: a
    }
    deriving (Eq, Ord, Show, Data, Typeable)
    
instance HkAnnotated HkTypeAliasDef where
  annot (HkTypeAliasDef _ _ _ _ a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Record Definition
--
-- A record is essentially a c struct, that can inherit other structs.
type HkRecDefNode = HkRecDef NodeInfo  
data HkRecDef a
  = HkRecDef
    { rec_name    :: HkName a
    , rec_tyvars  :: [HkName a]
    , rec_supers  :: [HkType a]
    , rec_context :: HkTypeContext a
    , rec_fields  :: [HkFieldDef a]
    , rec_annot   :: a
    }
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkRecDef where
  annot (HkRecDef _ _ _ _ _ a) = a

-- | Record Field Definition
type HkFieldDefNode = HkFieldDef NodeInfo
data HkFieldDef a
  = HkFieldDef [HkName a] (HkType a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkFieldDef where
  annot (HkFieldDef _ _ a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Data Definition
--
-- A data definition in Hawk is used to construct sum and product types.
-- This is effectively a union.
type HkDataDefNode = HkDataDef NodeInfo  
data HkDataDef a
  = HkDataDef
    { dat_name    :: HkName a
    , dat_tyvars  :: [HkName a]
    , dat_context :: HkTypeContext a
    , dat_cons    :: [HkConDef a]
    , dat_annot   :: a
    }
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkDataDef where
  annot (HkDataDef _ _ _ _ a) = a

-- | Data constructor definition
type HkConDefNode = HkConDef NodeInfo  
data HkConDef a
  = HkConDef (HkName a) [HkType a] a
  | HkConRecDef (HkName a) [HkFieldDef a] a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkConDef where
  annot (HkConDef _ _ a) = a
  annot (HkConRecDef _ _ a) = a
 
-- -----------------------------------------------------------------------------  
-- | Hawk Class
--
-- A class in hawk is a named list of functions that operate on a generalized type.
type HkClassDefNode = HkClassDef NodeInfo
data HkClassDef a
  = HkClassDef
  { class_name      :: HkName a
  , class_tyvars    :: [HkName a]
  , class_context   :: HkTypeContext a
  , class_body      :: [HkClassMember a]
  , class_annot     :: a
  }
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkClassDef where
  annot (HkClassDef _ _ _ _ a) = a
  

-- A class member is a function that is either declared, or defined.
-- If defined, that function serves as the default unless overriden.
type HkClassMemberNode = HkClassMember NodeInfo
data HkClassMember a
  = HkClassMember (HkVisibilityTag a) (HkFn a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkClassMember where
  annot (HkClassMember _ _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Class Instance
-- 
-- A class instance is implements the class for the given type.
-- This allows that type to match a class constraint.
type HkClassInstDefNode = HkClassInstDef NodeInfo
data HkClassInstDef a
  = HkClassInstDef
  { inst_class      :: HkName a
  , inst_tyargs     :: [HkType a]
  , inst_context    :: HkTypeContext a  
  , inst_body       :: [HkClassInstMember a]
  , inst_annot      :: a
  }
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkClassInstDef where
  annot (HkClassInstDef _ _ _ _ a) = a
  
-- A class member is a function that is either declared, or defined.
-- If defined, that function serves as the default unless overriden.
type HkClassInstMemberNode = HkClassInstMember NodeInfo
data HkClassInstMember a
  = HkClassInstMember (HkVisibilityTag a) (HkFn a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkClassInstMember where
  annot (HkClassInstMember _ _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Block
--
-- A block is a list of statements, which are internal to functions.
type HkBlockNode = HkBlock NodeInfo 
data HkBlock a
  = HkBlock [HkBlockStmt a] a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkBlock where
  annot (HkBlock _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Block Statement
--
-- These are the valid statements that a block may contain.
type HkBlockStmtNode = HkBlockStmt NodeInfo
data HkBlockStmt a
  = HkStmtBlock (HkBlock a) a
  | HkStmtExp (HkExp a) a
  
  | HkStmtBind (HkBind a)
  | HkStmtAssign (HkQName a) (HkExp a) a
  
  | HkStmtReturn (HkExp a) a
  
  | HkStmtMatch (HkExp a) [HkMatch a] a
  
  | HkStmtIf (HkExp a) (HkBlock a) (HkBlockStmt a) a
  | HkStmtElse (HkBlock a) a
  
  | HkStmtWhile (HkExp a) (HkBlock a) a
  | HkStmtDoWhile (HkExp a) (HkBlock a) a
  
  | HkStmtFor       (Maybe (HkForInit a))   (Maybe (HkExp a))   (Maybe (HkExp a))  (HkBlock a) a
  | HkStmtForEach   (HkName a) (HkExp a)  (HkBlock a) (HkBlock a) a
  | HkStmtForEachIx (HkName a) (HkName a) (HkExp a)   (HkBlock a) (HkBlock a) a
  
  | HkStmtEmpty a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkBlockStmt where
  annot (HkStmtBlock _ a) = a
  annot (HkStmtExp _ a) = a
  
  annot (HkStmtBind n) = annot n
  annot (HkStmtAssign _ _ a) = a
  
  annot (HkStmtReturn _ a) = a
  
  annot (HkStmtMatch _ _ a) = a
  
  annot (HkStmtIf _ _ _ a) = a
  annot (HkStmtElse _ a) = a
  
  annot (HkStmtWhile _ _ a) = a
  annot (HkStmtDoWhile _ _ a) = a
  
  annot (HkStmtFor _ _ _ _ a) = a
  annot (HkStmtForEach _ _ _ _ a) = a
  annot (HkStmtForEachIx _ _ _ _ _ a) = a
  
  annot (HkStmtEmpty a) = a
  

data HkForInit a
  = HkForLocalVars [HkBind a] a 
  | HkForInitExps  [HkExp a] a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkForInit where
  annot (HkForLocalVars _ a) = a
  annot (HkForInitExps _ a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Expression
--
-- These are the valid expressions that Hawk can evaluate during runtime.
type HkExpNode = HkExp NodeInfo
data HkExp a
  = HkConstExp    (HkConst a)
  | HkExpVar      (HkQName a)
  | HkExpCon      (HkQName a)
  
  | HkExpPrefixApp (HkUnaryOp a) (HkExp a) a
  | HkExpInfixApp (HkExp a) (HkBinaryOp a) (HkExp a) a

  | HkExpApp (HkExp a) (HkExp a) a
  | HkExpCast (HkExp a) (HkQualType a) a
  
  | HkExpIfThenElse (HkExp a) (HkExp a) (HkExp a) a
  
  | HkExpLambda [HkPattern a] (HkExp a) a
  
  | HkExpDo (HkBlock a) a
  
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkExp where
  annot (HkConstExp n) = annot n
  annot (HkExpVar n) = annot n
  annot (HkExpCon n) = annot n
  
  annot (HkExpPrefixApp _ _ a) = a
  annot (HkExpInfixApp _ _ _ a) = a
  
  annot (HkExpApp _ _ a) = a
  annot (HkExpCast _ _ a) = a
  
  annot (HkExpIfThenElse _ _ _ a) = a
  
  annot (HkExpLambda _ _ a) = a
  annot (HkExpDo _ a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Constants
--
-- These are the constant (i.e. primitive) objects in Hawk.
type HkConstNode = HkConst NodeInfo  
data HkConst a
  = HkUnit a
  | HkBool Bool  a
  | HkW8  Integer a
  | HkW16 Integer a
  | HkW32 Integer a
  | HkW64 Integer a
  | HkI8  Integer a
  | HkI16 Integer a
  | HkI32 Integer a
  | HkI64 Integer a
  | HkF32  Double a
  | HkF64  Double a
  | HkChar Char a
  | HkString String a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkConst where
  annot (HkUnit a) = a
  annot (HkBool _ a) = a
  
  annot (HkW8 _ a) = a
  annot (HkW16 _ a) = a
  annot (HkW32 _ a) = a
  annot (HkW64 _ a) = a
  
  annot (HkI8 _ a) = a
  annot (HkI16 _ a) = a
  annot (HkI32 _ a) = a
  annot (HkI64 _ a) = a
  
  annot (HkF32 _ a) = a
  annot (HkF64 _ a) = a
  
  annot (HkChar _ a) = a
  annot (HkString _ a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Assign Operators
--
-- These are built-in assignment operators.
type HkAssignOpNode = HkAssignOp NodeInfo
data HkAssignOp a
  = HkAssignOp a  -- Assignment
  | HkMulAssOp a  -- Multiplication
  | HkDivAssOp a  -- Division
  | HkRmdAssOp a  -- Remainder of Division
  | HkAddAssOp a  -- Addition
  | HkSubAssOp a  -- Subtraction
  | HkShlAssOp a  -- Bitshift Left
  | HkShrAssOp a  -- Bitshift Right
  | HkAndAssOp a  -- Bitwise And
  | HkXorAssOp a  -- Exclusive Bitwise Or
  | HkOrAssOp  a  -- Inclusive Bitwise Or
  deriving (Eq, Ord, Show, Data, Typeable)  

instance HkAnnotated HkAssignOp where
  annot (HkAssignOp a) = a
  annot (HkMulAssOp a) = a
  annot (HkDivAssOp a) = a
  annot (HkRmdAssOp a) = a
  annot (HkAddAssOp a) = a
  annot (HkSubAssOp a) = a
  annot (HkShlAssOp a) = a
  annot (HkShrAssOp a) = a
  annot (HkAndAssOp a) = a
  annot (HkXorAssOp a) = a
  annot (HkOrAssOp a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Binary Operators
--
-- These are built-in binary operators.
type HkBinaryOpNode = HkBinaryOp NodeInfo
data HkBinaryOp a
  = HkMulOp a   -- Multiply
  | HkDivOp a   -- Divide
  | HkRmdOp a   -- Remainder of Division
  | HkAddOp a   -- Addition
  | HkSubOp a   -- Subtraction
  | HkShlOp a   -- Shift Left
  | HkShrOp a   -- Shift Right
  | HkLeOp  a   -- Lesser Than
  | HkGrOp  a   -- Greater Than
  | HkLeqOp a   -- Lesser Than or Equal To
  | HkGeqOp a   -- Greater Than or EqualTo
  | HkEqOp  a   -- Equal To
  | HkNeqOp a   -- Not Equal To
  | HkAndOp a   -- Bitwise And
  | HkXorOp a   -- Exclusive Bitwise Or
  | HkOrOp  a   -- Inclusive Bitwise Or
  | HkLndOp a   -- Logical And
  | HkLorOp a   -- Logical Or
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkBinaryOp where
  annot (HkMulOp a) = a
  annot (HkDivOp a) = a
  annot (HkRmdOp a) = a
  annot (HkAddOp a) = a
  annot (HkSubOp a) = a
  annot (HkShlOp a) = a
  annot (HkShrOp a) = a
  annot (HkLeOp a) = a
  annot (HkGrOp a) = a
  annot (HkLeqOp a) = a
  annot (HkGeqOp a) = a
  annot (HkEqOp a) = a
  annot (HkNeqOp a) = a
  annot (HkAndOp a) = a
  annot (HkXorOp a) = a
  annot (HkOrOp a) = a
  annot (HkLndOp a) = a
  annot (HkLorOp a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Unary Operators
--
-- These are built-in unary operators.  
type HkUnaryOpNode = HkUnaryOp NodeInfo
data HkUnaryOp a
  = HkPreIncOp a    -- Prefix Increment
  | HkPreDecOp a    -- Prefix Decrement
  | HkPostIncOp a   -- Postfix Increment
  | HkPostDecOp a   -- Postfix Decrement
  | HkAddrOp a      -- Address Operator
  | HkIndOp a       -- Indirection Operator
  | HkPlusOp a      -- Prefix plus
  | HkMinOp a       -- Prefix minus
  | HkCompOp a      -- One's Complement
  | HkNegOp a       -- Logical Negation
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkUnaryOp where
  annot (HkPreIncOp a) = a
  annot (HkPreDecOp a) = a
  annot (HkPostIncOp a) = a
  annot (HkPostDecOp a) = a
  annot (HkAddrOp a) = a
  annot (HkIndOp a) = a
  annot (HkPlusOp a) = a
  annot (HkMinOp a) = a
  annot (HkCompOp a) = a
  annot (HkNegOp a) = a
  
-- ----------------------------------------------------------------------------- 
-- Hawk Identifiers
  
-- | A path of modules is composed of a list of module names.
type HkModPathNode = HkModPath NodeInfo
data HkModPath a
  = HkModPath [HkName a] a
  deriving (Eq, Ord, Data, Typeable)
  
instance Show (HkModPath a) where
  show (HkModPath ns _)
    = foldl' (\l n -> l ++ show n) [] ns

instance HkAnnotated HkModPath where
  annot (HkModPath _ a) = a

-- | A qualified name is used to represent qualified functions, variables, and
-- constructors.
type HkQNameNode = HkQName NodeInfo
data HkQName a
  = HkQual (HkModPath a) (HkName a) a     -- ^ name qualiied with a module name
  | HkUnQual (HkName a)                   -- ^ unqualified name
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkQName where
  annot (HkQual _ _ a) = a
  annot (HkUnQual n) = annot n

-- | A name represents variables, types and constructors.
type HkNameNode = HkName NodeInfo
data HkName a
  = HkIdent !String a     -- ^ /varid/ or /conid/
  | HkSymbol !String a    -- ^ /varsym/ or /consym/
  deriving (Eq, Ord, Data, Typeable)

instance Show (HkName a) where
  show (HkIdent s _) = s
  show (HkSymbol s _) = "(" ++ s ++ ")"

instance HkAnnotated HkName where
  annot (HkIdent _ a) = a
  annot (HkSymbol _ a) = a
  
-- | Used for infix operators that can be qualified, appearing in expressions.
type HkQOpNode = HkQOp NodeInfo
data HkQOp a
  = HkQVarOp (HkQName a)  -- ^ Variable Operator /(qvarop)/
  | HkQConOp (HkQName a)  -- ^ Constructor Operator /(qconop)/
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkQOp where
  annot (HkQVarOp n) = annot n
  annot (HkQConOp n) = annot n

-- | Operators, which appear in infix declarations.
type HkOpNode = HkOp NodeInfo
data HkOp a
  = HkVarOp (HkName a)  -- ^ variable operator /(varop)/
  | HkConOp (HkName a)  -- ^ constructor operator /(conop)/
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkOp where
  annot (HkVarOp n) = annot n
  annot (HkConOp n) = annot n
  
-- | A name of a component of a class or data type.
type HkCNameNode = HkCName NodeInfo
data HkCName a
  = HkVarName (HkName a)    -- ^ name of a method or field
  | HkConName (HkName a)    -- ^ name of a data constructor
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkCName where
  annot (HkVarName n) = annot n
  annot (HkConName n) = annot n