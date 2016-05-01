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
  = HkRootModule (HkModPath a) [HkExtStmt a] a
  deriving (Eq, Ord, Show, Data, Typeable)
    
instance HkAnnotated HkRootModule where
  annot (HkRootModule _ _ a) = a

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

-- -----------------------------------------------------------------------------
-- | Hawk External Statement
--
type HkExtStmtNode = HkExtStmt NodeInfo
data HkExtStmt a
  -- Module definition statement, defaults to public.
  = HkModDef (HkVisibilityTag a) (HkModPath a) (HkExtBlock a) a

  -- Import statment, may be qualified or unqualified
  -- By default, an import statement is private.
  | HkExtImport     (HkVisibilityTag a) (HkImportPath a) a
  | HkExtImportQual (HkVisibilityTag a) (HkImportPath a) a
  
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
type HkFnDecNode = HkFnDec NodeInfo
data HkFnDec a
  = HkFnDec
    { fn_name     :: HkName a
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
  = HkPatIdent  (HkName a) a
  | HkPatConst  (HkConst a) a
  | HkPatRec    (HkQName a) [HkPattern a] a
  | HkPatTuple  [HkPattern a] a
  | HkPatAlias  (HkName a) (HkPattern a) a
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
  { obj_name    :: HkName a
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
  | HkStmtAssign (HkQName a) (HkExp a) a
  
  | HkStmtReturn (HkExp a) a
  
  | HkStmtCase (HkExp a) [HkBinding a] a
  
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
  
  annot (HkStmtValDef _ a) = a
  annot (HkStmtVarDec _ a) = a
  annot (HkStmtVarDef _ a) = a
  annot (HkStmtAssign _ _ a) = a
  
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