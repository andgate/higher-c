{-# LANGUAGE FlexibleInstances #-}
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
    deriving (Eq, Ord, Show)
    
instance HkAnnotated HkTranslUnit where
  annot (HkTranslUnit _ a) = a

  
type HkRootModuleNode = HkRootModule NodeInfo
data HkRootModule a
  = HkRootModule (HkDottedIdent a) [HkExtStmt a] a
    deriving (Eq, Ord, Show)
    
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
  deriving (Eq, Ord, Show)

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
    
  deriving (Eq, Ord, Show)


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
  

type HkExtBlockNode = HkExtBlock NodeInfo
data HkExtBlock a
  = HkExtBlock [HkExtStmt a] a
  deriving (Eq, Ord, Show)
  
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
  deriving (Eq, Ord, Show)
  
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
  deriving (Eq, Ord, Show)

instance HkAnnotated HkImportItem where
  annot (HkImportItem _ _ a) = a

prefixImportItems :: (Monoid a) => HkDottedIdent a -> HkImportItems a -> HkImportItems a
prefixImportItems i = map (prefixImportItem i)

prefixImportItem :: (Monoid a) => HkDottedIdent a -> HkImportItem a -> HkImportItem a
prefixImportItem pfx imp_item@(HkImportItem ident _ _) 
  =  imp_item { import_item = pfx ++ ident }

-- -----------------------------------------------------------------------------
-- | Hawk Type Signature
--
-- A type signature contains the type of a variable or function. A type signature represents
-- a mapping from @sig_params@ to @sig_result@. A type signature can be restricted by a type context.
type HkTypeSigNode = HkTypeSig NodeInfo
data HkTypeSig a
  = HkTypeSig
  { sig_context  :: HkTypeContext a
  , sig_params   :: [HkType a]
  , sig_result   :: HkType a
  , sig_annot    :: a
  }
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------
-- | Hawk Type
--
-- A type in Hawk can be primitive type, reference types, a record type, or even a type signature.
type HkTypeNode = HkType NodeInfo
data HkType a
  = HkTyPrimType    (HkPrimType a) a
  | HkTyConst       (HkType a) a
  | HkTyRefType     (HkRefType a) a
  | HkTyRecordType  (HkRecordType a) a
  | HkTyTypeSig     (HkTypeSig a) a
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------
-- | Hawk Reference Type
--
-- A reference type is an address to some data. This include pointers, arrays, and type variables.
type HkRefTypeNode = HkRefType NodeInfo
data HkRefType a
  = HkRefType (HkType a) a
  | HkArrayType (HkType a) a
  | HkTupleType [HkType a] a
  | HkTypeVariable (HkIdent a) a
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------
-- | Hawk Record Type
--
-- A record type is a reference to a record defined in program. Records should be
-- constructed if a record is defined with type parameters.
type HkRecordTypeNode = HkRecordType NodeInfo
data HkRecordType a
  = HkRecordType  (HkIdent a) a
  | HkRecordCons  (HkIdent a) [HkType a] a
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------
-- | Hawk Type Context
--
-- A type context provides type class restrictions to type variables, which allows a type
-- variable to support multiple interfaces.
type HkTypeContextNode = HkTypeContext NodeInfo
data HkTypeContext a
  = HkTypeContext [HkClassCons a] a
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------
-- | Hawk Function Declaration
--
-- A function declaration is the header of a function, containing the name, args, and type signature of a function
type HkFnDecNode = HkFnDec NodeInfo
data HkFnDec a
  = HkFnDec
    { fn_symbol   :: HkFnSymbol a
    , fn_typesig  :: HkTypeSig a
    , fn_annot    :: a
    }
    deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------
-- | Hawk Function definition
--
-- A function definition consists of a function declarations along with a block that serves as the body.
type HkFnDefNode = HkFnDef NodeInfo
data HkFnDef a
  = HkFnDef (HkFnDec a) [HkBinding a] a
  deriving (Eq, Ord, Show)
  
  
type HkFnSymbolNode = HkFnSymbol NodeInfo
data HkFnSymbol a
  = HkSymIdent     (HkIdent a) a
  | HkSymPreOp  (HkIdent a) Int a
  | HkSymOp   (HkIdent a) Int a
  | HkSymPostOp (HkIdent a) Int a
  deriving (Eq, Ord, Show)
  
type HkBindingNode = HkBinding NodeInfo
data HkBinding a
  = HkBinding
  { binding_params  :: [HkPattern a]
  , binding_blocks  :: [(HkGuard a, HkBlock a)]
  , binding_annot   :: a
  }
  deriving (Eq, Ord, Show)

type HkPatternNode = HkPattern NodeInfo
data HkPattern a
  = HkPatIdent  (HkIdent a) a
  | HkPatConst  (HkConst a) a
  | HkPatRec    (HkIdent a) [HkPattern a] a
  | HkPatTyple  [HkPattern a] a
  | HkPatAlias  (HkIdent a) (HkPattern a) a
  | HkPatAny    a
  deriving (Eq, Ord, Show)

type HkGuardNode = HkGuard NodeInfo
data HkGuard a
  = HkGuardExp (HkExp a) a
  | HkGuardAny a
  deriving (Eq, Ord, Show)


-- -----------------------------------------------------------------------------
-- | Hawk Object Declaration
--
-- An object declaration declares a variable or value with a name and a type
--
-- Objects in Hawk are not like objects in most programming languages.
-- Hawk objects are variables or values that represent some data in memory.
-- Hawk objects have nothing to do with object oriented programming. 
type HkObjectDecNode = HkObjectDec NodeInfo
data HkObjectDec a
  = HkBindingDec
  { obj_name    :: HkIdent a
  , obj_typesig :: [HkIdent a]
  , obj_annot   :: a
  }
  deriving (Eq, Ord, Show)
  

-- -----------------------------------------------------------------------------
-- | Hawk Object Definition
--
-- A object Definition is used to bind an identifier to an expression.
type HkObjectDefNode = HkObjectDef NodeInfo
data HkObjectDef a
  = HkObjectDef (HkObjectDef a) (HkExp a) a
  deriving (Eq, Ord, Show)


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
  = HkValDec (HkObjectDec a) a
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------
-- | Hawk Value Definiton
--
-- A hawk value represents an immutable binding (i.e. cannot be changed).
--
-- A hawk value definition is a contains a binding definition.
type HkValDefNode = HkValDef NodeInfo
data HkValDef a
  = HkValDef (HkObjectDef a) a
  deriving (Eq, Ord, Show)

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
  = HkVarDec (HkObjectDec a) a
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------  
-- | Hawk Variable Definition
--
-- A variable represents a mutable binding (i.e. can be changed).
--
-- A hawk variable definition contains a binding definition.
--
type HkVarDefNode = HkVarDef NodeInfo
data HkVarDef a
  = HkVarDef (HkObjectDef a) a
  deriving (Eq, Ord, Show)
  
-- -----------------------------------------------------------------------------  
-- | Hawk Record Definition
--
-- Records in hawk contain variables and values.
type HkRecordDefNode = HkRecordDef NodeInfo
data HkRecordDef a
  = HkRecordDef
    { rec_name    :: HkIdent a
    , rec_tyvars  :: [HkIdent a]
    , rec_ctx     :: HkTypeContext a
    , rec_supers  :: [HkRecordType a]
    , rec_mems    :: [HkRecordMember a]
    , rec_annot   :: a
    }

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
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------
-- | Hawk Union Definition
--
-- Unions in hawk are classified as tagged unions. Each element of these unions
-- have a tag, which are enormously helpful for typesafety and pattern matching.
-- It's a minor, minor overhead, and they perform as well as tagless unions.
type HkUnionDefNode = HkUnionDef NodeInfo
data HkUnionDef a
  = HkUnion
  { union_name    :: HkIdent a
  , union_ctx     :: HkTypeContext a
  , union_tyvars  :: [HkIdent a]
  , union_elems   :: [HkUnionElement a]
  , union_annot   :: a
  }
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------  
-- Element belonging to a union consists of a tag and a list of types.  
type HkUnionElementNode = HkUnionElement NodeInfo
data HkUnionElement a
  = HkUnionElement (HkIdent a) [HkType a] a
  deriving (Eq, Ord, Show)
  
-- -----------------------------------------------------------------------------  
-- | Hawk Class
--
-- A class in hawk is a named list of functions that operate on a generalized type.
type HkClassDefNode = HkClassDef NodeInfo
data HkClassDef a
  = HkClassDef
  { class_name      :: HkIdent a
  , class_tyctx     :: HkTypeContext a
  , class_tyvars    :: [HkIdent a]  
  , class_body      :: [HkClassMember a]
  , class_annot     :: a
  }
  deriving (Eq, Ord, Show)

-- A class member is a function that is either declared, or defined.
-- If defined, that function serves as the default unless overriden.
type HkClassMemberNode = HkClassMember NodeInfo
data HkClassMember a
  = HkClassMemberDec (HkVisibilityTag a) (HkFnDec a) a
  | HkClassMemberDef (HkVisibilityTag a) (HkFnDef a) a
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------
-- | Hawk Class Instance
-- 
-- A class instance is implements the class for the given type.
-- This allows that type to match a class constraint.
type HkClassInstanceNode = HkClassInstance NodeInfo
data HkClassInstance a
  = HkClassInstance
  { inst_class       :: HkIdent a
  , inst_tyargs      :: [HkType a]
  , inst_body        :: [HkFnDef a]
  , inst_annot       :: a
  }
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------
-- | Hawk Block
--
-- A block is a list of statements, which are internal to functions.
type HkBlockNode = HkBlock NodeInfo 
data HkBlock a
  = HkBlock [HkBlockStmt a] a
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------
-- | Hawk Block Statement
--
-- These are the valid statements that a block may contain.
type HkBlockStmtNode = HkBlockStmt NodeInfo
data HkBlockStmt a
  = HkStmtBlk (HkBlockStmt a) a
  | HkStmtExp (HkExp a) a
  
  | HkStmtValDef (HkValDef a) a
  | HkStmtVarDec (HkVarDec a) a
  | HkStmtVarDef (HkVarDef a) a
  
  | HkStmtReturn (HkExp a) a
  
  | HkStmtCase (HkExp a) [HkBinding a] a
  
  | HkStmtIf (HkExp a) (HkBlock a) a
  | HkStmtIfElse (HkExp a) (HkBlock a) (HkBlock a) a
  
  | HkStmtWhile (HkExp a) (HkBlock a) a
  | HkStmtDoWhile (HkExp a) (HkBlock a) a
  
  | HkStmtFor       (Maybe (HkForInit a))   (Maybe (HkExp a))   (Maybe (HkExp a))   (HkBlock a) a
  | HkStmtForEach   (HkIdent a) (HkExp a)   (HkBlock a) (HkBlock a) a
  | HkStmtForEachIx (HkIdent a) (HkIdent a) (HkExp a)   (HkBlock a) (HkBlock a) a
  
  | HkStmtEmpty a
  deriving (Eq, Ord, Show)
  
data HkForInit a
  = HkForLocalVars [HkVarDec a] a 
  | HkForInitExps  [HkExp a] a
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------  
-- | Hawk Expression
--
-- These are the valid expressions that Hawk can evaluate during runtime.
type HkExpNode = HkExp NodeInfo
data HkExp a
  = HkConstExpr   (HkConst a) a
  | HkExpAssign   (HkAssignOp a) (HkIdent a) (HkExp a) a
  | HkExpUnaryOp  (HkUnaryOp a) (HkExp a) a
  | HkExpBinaryOp (HkBinaryOp a) (HkExp a) (HkExp a) a
  
  | HkExpObj  (HkIdent a) a
  | HkExpCall (HkExp a) [HkExp a] a
  | HkExpCast (HkExp a) (HkTypeSig a) a
  
  | HkExpIfThenElse (HkExp a) (HkExp a) (HkExp a) a
  
  | HkExpLambda (HkBinding a) a
  
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)
  

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
  deriving (Eq, Ord, Show)  

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
  deriving (Eq, Ord, Show)  
  
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
  deriving (Eq, Ord, Show)