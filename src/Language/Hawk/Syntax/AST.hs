{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Language.Hawk.Syntax.AST where

import Language.Hawk.Data.Node

import Data.Int
import Data.Word

import Data.Generics
import Data.Monoid

import Control.Lens


class HkAnnotated t where
  annot :: t a -> a
  
instance (HkNode a, HkAnnotated t) => HkNode (t a) where
  nodeInfo = nodeInfo . annot

-- -----------------------------------------------------------------------------
-- | Complete Hawk translation unit
--
-- A complete Hawk translation unit, for example representing all Hawk source or info files of a program.
-- It consists of a list of modules, which contain all the external statements.
type HkTranslUnitNode = HkTranslUnit NodeInfo
data HkTranslUnit a
  = HkTranslUnit (HkRootModule a)  a
    deriving (Eq, Ord, Show, Data, Typeable)
    
instance HkAnnotated HkTranslUnit where
  annot (HkTranslUnit _ a) = a

  
type HkRootModuleNode = HkRootModule NodeInfo
data HkRootModule a
  = HkRootModule (HkDottedIdent a) [HkExtStmt a] a
    deriving (Eq, Ord, Show, Data, Typeable)
    
instance HkAnnotated HkRootModule where
  annot (HkRootModule _ _ a) = a

-- ----------------------------------------------------------------------------- 
-- | Hawk Identifier
--
-- An identifier in hawk is used to contain the names of functions, types,
--  variables, and modules.
--
type HkIdentNode = HkIdent NodeInfo
data HkIdent a
  = HkIdent !String a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkIdent where
  annot (HkIdent _ a) = a

-- Dotted identifiers are used for module names, and for qualified values and functions.
type HkDottedIdentNode = HkDottedIdent NodeInfo
type HkDottedIdent a = [HkIdent a]


-- -----------------------------------------------------------------------------
-- | Hawk External Statement
--
type HkExtStmtNode = HkExtStmt NodeInfo
data HkExtStmt a
  -- Module definition statement, defaults to public.
  = HkModDef (HkVisibilityTag a) (HkDottedIdent a) (HkExtBlock a) a

  -- Import statment, may be qualified or unqualified
  -- By default, an import statement is private.
  | HkExtImport     (HkVisibilityTag a) (HkImportItems a) a
  | HkExtImportQual (HkVisibilityTag a) (HkImportItems a) a
  
  -- External function statements
  -- At the external level, a function can be linked, declared, or defined.
  -- A Function at the external level also requires a visibility tag.
  | HkExtFnLink (HkVisibilityTag a) (HkFnDec a) a
  | HkExtFnDec  (HkVisibilityTag a) (HkFnDec a) a
  | HkExtFnDef  (HkVisibilityTag a) (HkFnDef a) a
  
  -- External binding statement
  -- These also come with visibility modifiers.
  -- External values must be defined.
  | HkExtValDef (HkVisibilityTag a) (HkValDef a) a
  -- External variables may be declared or defined.
  | HkExtVarDec (HkVisibilityTag a) (HkVarDec a) a
  | HkExtVarDef (HkVisibilityTag a) (HkVarDef a) a
  
  | HkExtTypeDef (HkVisibilityTag a) (HkTypeDef a) a
    
  deriving (Eq, Ord, Show, Data, Typeable)


instance HkAnnotated HkExtStmt where
  annot (HkModDef _ _ _ a) = a
  annot (HkExtImport _ _ a) = a
  annot (HkExtImportQual _ _ a) = a
  annot (HkExtFnLink _ _ a) = a
  
  annot (HkExtFnDec _ _ a) = a
  annot (HkExtFnDef _ _ a) = a
  
  annot (HkExtValDef _ _ a) = a
  annot (HkExtVarDec _ _ a) = a
  annot (HkExtVarDef _ _ a) = a
  
  annot (HkExtTypeDef _ _ a) = a
  

type HkExtBlockNode = HkExtBlock NodeInfo
data HkExtBlock a
  = HkExtBlock [HkExtStmt a] a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkExtBlock where
  annot (HkExtBlock _ a) = a


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
-- | Hawk Import Items
--
-- Import items in hawk are a list of dotted identifiers to some module or
-- external function, record, or variable.
-- 
-- Import items are provided an ident that can overrides their current qualified alias.
-- This can be useful for shorthanded qualified names. An empty alias override will be ignored.
type HkImportItemsNode = HkImportItems NodeInfo  
type HkImportItems a = [HkImportItem a]
  
type HkImportItemNode = HkImportItem NodeInfo  
data HkImportItem a
  = HkImportItem 
    { import_item   :: HkDottedIdent a
    , import_alias  :: Maybe (HkIdent a)
    , import_annot  :: a
    }
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkImportItem where
  annot (HkImportItem _ _ a) = a

prefixImportItems :: (Monoid a) => HkDottedIdent a -> HkImportItems a -> HkImportItems a
prefixImportItems i = map (prefixImportItem i)

prefixImportItem :: (Monoid a) => HkDottedIdent a -> HkImportItem a -> HkImportItem a
prefixImportItem pfx imp_item@(HkImportItem ident _ _) 
  =  imp_item { import_item = pfx ++ ident }

importItemsAlias :: Maybe (HkIdent a) -> HkImportItems a -> HkImportItems a
importItemsAlias a = map (\ii -> ii { import_alias = a })
  
prefixImportItemsAlias :: (Monoid a) => HkDottedIdent a -> Maybe (HkIdent a) -> HkImportItems a -> HkImportItems a
prefixImportItemsAlias i a = map (prefixImportItemAlias i a)  
  
prefixImportItemAlias :: (Monoid a) => HkDottedIdent a -> Maybe (HkIdent a) -> HkImportItem a -> HkImportItem a
prefixImportItemAlias pfx alias imp_item@(HkImportItem ident _ _) 
  =  imp_item { import_item = pfx ++ ident, import_alias = alias }


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

-- | Type context
type HkTypeContextNode = HkTypeContext NodeInfo
type HkTypeContext a = [HkClassAsst a]

-- | Class assertions
type HkClassAsstNode = HkClassAsst NodeInfo
data HkClassAsst a
  = HkClassAsst (HkDottedIdent a) [HkType a] a
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
  | HkTyCon         (HkDottedIdent a) a
  | HkTyVar         (HkIdent a) a
  
  | HkTyConst       (HkType a) a
  | HkTyRef         (HkType a) a
  | HkTyArray       (HkType a) a
  | HkTyTuple       [HkType a] a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkType where
  annot (HkTyFun _ _ a) = a
  annot (HkTyApp _ _ a) = a
  
  annot (HkTyPrim _ a) = a
  annot (HkTyCon _ a) = a
  annot (HkTyVar _ a) = a
  
  annot (HkTyConst _ a) = a
  annot (HkTyRef _ a) = a
  annot (HkTyArray _ a) = a
  annot (HkTyTuple _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Primitive Type
--
-- The primitive types in Hawk. 
type HkPrimTypeNode = HkPrimType NodeInfo
data HkPrimType a
  = HkTyUnit a
  | HkTyBit a
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
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkPrimType where
  annot (HkTyUnit a) = a
  annot (HkTyBit a) = a
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
  { class_cons_name     :: HkDottedIdent a
  , class_cons_tyvars   :: [HkIdent a]
  , class_cons_annot    :: a
  }
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkClassCons where
  annot (HkClassCons _ _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Function Declaration
--
-- A function declaration is the header of a function, containing the name, args, and type signature of a function
type HkFnDecNode = HkFnDec NodeInfo
data HkFnDec a
  = HkFnDec
    { fn_symbol   :: HkFnSymbol a
    , fn_type     :: HkQualType a
    , fn_annot    :: a
    }
    deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkFnDec where
  annot (HkFnDec _ _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Function definition
--
-- A function definition consists of a function declarations along with a block that serves as the body.
type HkFnDefNode = HkFnDef NodeInfo
data HkFnDef a
  = HkFnDef (HkFnDec a) [HkBinding a] a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkFnDef where
  annot (HkFnDef _ _ a) = a


-- -----------------------------------------------------------------------------
-- | Hawk Function Symbol
--
-- A function symbol holds the function identifier. Function symbols can be 
-- a string of characters, or an operator symbol.
type HkFnSymbolNode = HkFnSymbol NodeInfo
data HkFnSymbol a
  = HkSymIdent  (HkIdent a) a
  | HkSymPreOp  (HkIdent a) Int a
  | HkSymOp     (HkIdent a) Int a
  | HkSymPostOp (HkIdent a) Int a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkFnSymbol where
  annot (HkSymIdent _ a) = a
  annot (HkSymPreOp _ _ a) = a
  annot (HkSymOp _ _ a) = a
  annot (HkSymPostOp _ _ a) = a  
  
-- -----------------------------------------------------------------------------
-- | Hawk Binding
--
-- This binds a set of patterns to a (guarded) block of code.
type HkBindingNode = HkBinding NodeInfo
data HkBinding a
  = HkBinding
  { binding_params  :: [HkPattern a]
  , binding_blocks  :: HkBindingBlock a
  , binding_annot   :: a
  }
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkBinding where
  annot (HkBinding _ _ a) = a
  
-- -----------------------------------------------------------------------------
-- | Hawk Patterns
--
-- Patterns are used to match data based on it's structure.
type HkPatternNode = HkPattern NodeInfo
data HkPattern a
  = HkPatIdent  (HkIdent a) a
  | HkPatConst  (HkConst a) a
  | HkPatRec    (HkDottedIdent a) [HkPattern a] a
  | HkPatTuple  [HkPattern a] a
  | HkPatAlias  (HkIdent a) (HkPattern a) a
  | HkPatAny    a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkPattern where
  annot (HkPatIdent _ a) = a
  annot (HkPatConst _ a) = a
  annot (HkPatRec _ _ a) = a
  annot (HkPatTuple _ a) = a
  annot (HkPatAlias _ _ a) = a
  annot (HkPatAny a) = a 
  

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

-- -----------------------------------------------------------------------------
-- | Hawk Binding block
--
-- Guards are a boolean expression that sits in front of a block of code.
type HkBindingBlockNode = HkBindingBlock NodeInfo
data HkBindingBlock a
  = HkBindingBlock (HkBlock a) a
  | HkBindingExp  (HkExp a) a
  | HkGuardedBindingBlock [HkGuardedBlock a] a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkBindingBlock where
  annot (HkBindingBlock _ a) = a
  annot (HkBindingExp _ a) = a
  annot (HkGuardedBindingBlock _ a) = a 

-- Binding blocks can be guarded
type HkGuardedBlockNode = HkGuardedBlock NodeInfo
data HkGuardedBlock a
  = HkGuardedBlock (HkGuard a) (HkBlock a) a
  | HkGuardedExp   (HkGuard a) (HkExp a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkGuardedBlock where
  annot (HkGuardedBlock _ _ a) = a
  annot (HkGuardedExp _ _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Object Declaration
--
-- An object declaration declares a variable or value with a name and a type
--
-- Objects in Hawk are not like objects in most programming languages.
-- Hawk objects are variables or values that represent some data in memory.
-- Hawk objects have nothing to do with object oriented programming. 
type HkObjDecNode = HkObjDec NodeInfo
data HkObjDec a
  = HkObjDec
  { obj_name    :: HkIdent a
  , obj_type    :: HkType a
  , obj_annot   :: a
  }
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkObjDec where
  annot (HkObjDec _ _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Object Definition
--
-- A object Definition is used to bind an identifier to an expression.
type HkObjDefNode = HkObjDef NodeInfo
data HkObjDef a
  = HkObjDef (HkObjDec a) (HkExp a) a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkObjDef where
  annot (HkObjDef _ _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Value Declaration
--
--
-- A hawk value represents an immutable binding (i.e. cannot be changed).
--
-- A Hawk value declaration is contains a binding declaration, and brings a value into existance.
--
-- Usually, value declarations are disallowed, as values cannot be changed and should be assigned
-- immediatly. However, some special cases, like record constructors, allow bindings to be declared
-- and given a value later.
--
type HkValDecNode = HkValDec NodeInfo
data HkValDec a
  = HkValDec (HkObjDec a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkValDec where
  annot (HkValDec _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Value Definiton
--
-- A hawk value represents an immutable binding (i.e. cannot be changed).
--
-- A hawk value definition is a contains a binding definition.
type HkValDefNode = HkValDef NodeInfo
data HkValDef a
  = HkValDef (HkObjDef a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkValDef where
  annot (HkValDef _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Variable Declaration
--
-- A variable represents a mutable binding (i.e. can be changed).
--
-- A hawk variable declaration contains a binding declaration.
--
-- A variable declaration will be initialized with the default value of it's type.
--
type HkVarDecNode = HkVarDec NodeInfo
data HkVarDec a
  = HkVarDec (HkObjDec a) a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkVarDec where
  annot (HkVarDec _ a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Variable Definition
--
-- A variable represents a mutable binding (i.e. can be changed).
--
-- A hawk variable definition contains a binding definition.
--
type HkVarDefNode = HkVarDef NodeInfo
data HkVarDef a
  = HkVarDef (HkObjDef a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkVarDef where
  annot (HkVarDef _ a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Type Definition
--
-- Types in hawk are either records, unions or type aliases.
type HkTypeDefNode = HkTypeDef NodeInfo
data HkTypeDef a
  = HkTyAliasDef  (HkTypeAliasDef a) a
  | HkTyRecDef    (HkRecordDef a) a
  | HkTyUnionDef  (HkUnionDef a) a
  | HkTyClassDef  (HkClassDef a) a
  | HkTyClassInstDef  (HkClassInstDef a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkTypeDef where
  annot (HkTyAliasDef _ a) = a
  annot (HkTyRecDef _ a) = a
  annot (HkTyUnionDef _ a) = a
  annot (HkTyClassDef _ a) = a
  annot (HkTyClassInstDef _ a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Type Alias Definition
--
-- A type alias in Hawk is a new type, that is defined based on another type.
type HkTypeAliasDefNode = HkTypeAliasDef NodeInfo
data HkTypeAliasDef a
  = HkTypeAliasDef
    { alias_name    :: HkIdent a
    , alias_tyvars  :: [HkIdent a]
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
-- Records in hawk contain variables and values.
type HkRecordDefNode = HkRecordDef NodeInfo
data HkRecordDef a
  = HkRecordDef
    { rec_name    :: HkIdent a
    , rec_tyvars  :: [HkIdent a]
    , rec_supers  :: [HkType a]
    , rec_context :: HkTypeContext a
    , rec_mems    :: [HkRecordMember a]
    , rec_annot   :: a
    }
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkRecordDef where
  annot (HkRecordDef _ _ _ _ _ a) = a
  
-- -----------------------------------------------------------------------------
-- | Hawk Record member
--
-- A record member in Hawk is a variable or a value, with a visibility tag.
--
-- The default visibility tag is public, which provides no restriction to member access. When set to
-- private, a record member can only be accessed by functions in a typeclass that the record belongs too.
--
-- A record can only contain value definitions, variable declarations, and variable definitions.
type HkRecordMemberNode = HkRecordMember NodeInfo
data HkRecordMember a
  = HkRecordValDef (HkVisibilityTag a) (HkValDef a) a
  | HkRecordVarDec (HkVisibilityTag a) (HkVarDec a) a
  | HkRecordVarDef (HkVisibilityTag a) (HkVarDef a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkRecordMember where
  annot (HkRecordValDef _ _ a) = a
  annot (HkRecordVarDec _ _ a) = a
  annot (HkRecordVarDef _ _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Union Definition
--
-- Unions in hawk are classified as tagged unions. Each element of these unions
-- have a tag, which are enormously helpful for typesafety and pattern matching.
-- It's a minor, minor overhead, and they perform as well as tagless unions.
type HkUnionDefNode = HkUnionDef NodeInfo
data HkUnionDef a
  = HkUnionDef
  { union_name    :: HkIdent a
  , union_tyvars  :: [HkIdent a]
  , union_context :: HkTypeContext a
  , union_elems   :: [HkUnionElement a]
  , union_annot   :: a
  }
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkUnionDef where
  annot (HkUnionDef _ _ _ _ a) = a

-- -----------------------------------------------------------------------------  
-- Element belonging to a union consists of a tag and a list of types.  
type HkUnionElementNode = HkUnionElement NodeInfo
data HkUnionElement a
  = HkUnionElement (HkIdent a) [HkType a] a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkUnionElement where
  annot (HkUnionElement _ _ a) = a
 
-- -----------------------------------------------------------------------------  
-- | Hawk Class
--
-- A class in hawk is a named list of functions that operate on a generalized type.
type HkClassDefNode = HkClassDef NodeInfo
data HkClassDef a
  = HkClassDef
  { class_name      :: HkIdent a
  , class_tyvars    :: [HkIdent a]
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
  = HkClassMemberDec (HkVisibilityTag a) (HkFnDec a) a
  | HkClassMemberDef (HkVisibilityTag a) (HkFnDef a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkClassMember where
  annot (HkClassMemberDec _ _ a) = a
  annot (HkClassMemberDef _ _ a) = a

-- -----------------------------------------------------------------------------
-- | Hawk Class Instance
-- 
-- A class instance is implements the class for the given type.
-- This allows that type to match a class constraint.
type HkClassInstDefNode = HkClassInstDef NodeInfo
data HkClassInstDef a
  = HkClassInstDef
  { inst_class      :: HkIdent a
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
  = HkClassInstMemberDef (HkVisibilityTag a) (HkFnDef a) a
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkClassInstMember where
  annot (HkClassInstMemberDef _ _ a) = a

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
  
  | HkStmtValDef (HkValDef a) a
  | HkStmtVarDec (HkVarDec a) a
  | HkStmtVarDef (HkVarDef a) a
  
  | HkStmtReturn (HkExp a) a
  
  | HkStmtCase (HkExp a) [HkBinding a] a
  
  | HkStmtIf (HkExp a) (HkBlock a) (HkBlockStmt a) a
  | HkStmtElse (HkBlock a) a
  
  | HkStmtWhile (HkExp a) (HkBlock a) a
  | HkStmtDoWhile (HkExp a) (HkBlock a) a
  
  | HkStmtFor       (Maybe (HkForInit a))   (Maybe (HkExp a))   (Maybe (HkExp a))  (HkBlock a) a
  | HkStmtForEach   (HkIdent a) (HkExp a)   (HkBlock a) (HkBlock a) a
  | HkStmtForEachIx (HkIdent a) (HkIdent a) (HkExp a)   (HkBlock a) (HkBlock a) a
  
  | HkStmtEmpty a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkBlockStmt where
  annot (HkStmtBlock _ a) = a
  annot (HkStmtExp _ a) = a
  
  annot (HkStmtValDef _ a) = a
  annot (HkStmtVarDec _ a) = a
  annot (HkStmtVarDef _ a) = a
  
  annot (HkStmtReturn _ a) = a
  
  annot (HkStmtCase _ _ a) = a
  
  annot (HkStmtIf _ _ _ a) = a
  annot (HkStmtElse _ a) = a
  
  annot (HkStmtWhile _ _ a) = a
  annot (HkStmtDoWhile _ _ a) = a
  
  annot (HkStmtFor _ _ _ _ a) = a
  annot (HkStmtForEach _ _ _ _ a) = a
  annot (HkStmtForEachIx _ _ _ _ _ a) = a
  
  annot (HkStmtEmpty a) = a

 
data HkForInit a
  = HkForLocalVars [HkVarDec a] a 
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
  = HkConstExp    (HkConst a) a
  | HkExpAssign   (HkAssignOp a) (HkIdent a) (HkExp a) a
  | HkExpUnaryOp  (HkUnaryOp a) (HkExp a) a
  | HkExpBinaryOp (HkBinaryOp a) (HkExp a) (HkExp a) a
  
  | HkExpObj  (HkIdent a) a
  | HkExpCall (HkExp a) [HkExp a] a
  | HkExpCast (HkExp a) (HkType a) a
  
  | HkExpIfThenElse (HkExp a) (HkExp a) (HkExp a) a
  
  | HkExpLambda (HkBinding a) a
  
  deriving (Eq, Ord, Show, Data, Typeable)
  
instance HkAnnotated HkExp where
  annot (HkConstExp _ a) = a
  annot (HkExpAssign _ _ _ a) = a
  annot (HkExpUnaryOp _ _ a) = a
  annot (HkExpBinaryOp _ _ _ a) = a
  
  annot (HkExpObj _ a) = a
  annot (HkExpCall _ _ a) = a
  annot (HkExpCast _ _ a) = a
  
  annot (HkExpIfThenElse _ _ _ a) = a
  
  annot (HkExpLambda _ a) = a

-- -----------------------------------------------------------------------------  
-- | Hawk Constants
--
-- These are the constant (i.e. primitive) objects in Hawk.
type HkConstNode = HkConst NodeInfo  
data HkConst a
  = HkUnit a
  | HkBit Bool a
  | HkW8  Word8 a
  | HkW16 Word16 a
  | HkW32 Word32 a
  | HkW64 Word64 a
  | HkI8  Int8 a
  | HkI16 Int16 a
  | HkI32 Int32 a
  | HkI64 Int64 a
  | HkF32 Float a
  | HkF64 Double a
  | HkChar Char a
  deriving (Eq, Ord, Show, Data, Typeable)

instance HkAnnotated HkConst where
  annot (HkUnit a) = a
  annot (HkBit _ a) = a
  
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
  | HkMinOp a       -- Postfix plus
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