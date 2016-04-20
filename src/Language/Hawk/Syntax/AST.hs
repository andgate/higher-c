{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}
module Language.Hawk.Syntax.AST where

import Language.Hawk.Data.Node
import Language.Hawk.Data.Position
import Data.Generics

type Name = String
type Ids  = [String]
type Params = [String]
type ModId = [String]
type Types = [String]
type Exprs = [Expr]

data Expr
  = IntExpr Int
  | FloatExpr Double
  | StringExpr String
  | UnaryOpExpr String Expr
  | BinaryOpExpr String Expr Expr
  | VarExpr String
  
  | ModuleExpr ModId Exprs
  | ImportExpr ModId Bool
  
  | VarDecExpr Name Types Expr
  | ValDecExpr Name Types Expr
  
  | FuncExpr Name Params Types Expr
  | ExternExpr Name Params Types
  
  | LetExpr Name Expr Expr
  | CallExpr Name Exprs
  
  | DoExpr Exprs
  | ReturnExpr Expr
  | IfExpr Expr Expr Expr
  | WhileExpr Expr Expr
  deriving (Eq, Show)
  
mkFuncExpr :: (Name, Params, Types) -> Expr -> Expr
mkFuncExpr (n, p, t) b = FuncExpr n p t b

mkExternExpr :: (Name, Params, Types) -> Expr
mkExternExpr (n, p, t) = ExternExpr n p t
  
  
-- | Hawk Identifier
--
-- An identifier in hawk is used to contain the names of functions, types,
--  variables, and modules.
--
type HkIdentNode = HkIdent NodeInfo
data HkIdent a
  = HkIdent !String a
    deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})


-- Dotted identifiers are used for module names, and for qualified values and functions.
type HkDottedIdentNode = HkDottedIdent NodeInfo
type HkDottedIdent a = [HkIdent a]

  
-- | Complete Hawk translation unit
--
-- A complete Hawk translation unit, for example representing all Hawk source or info files of a program.
-- It consists of a list of modules, which contain all the external statements.
type HkTranslUnitNode = HkTranslUnit NodeInfo
data HkTranslUnit a
  = HkTranslUnit [HkModuleDef a] a
    deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})


-- | Hawk Module definition
-- 
-- A module definition is a named list of external (i.e. toplevel) statements.
type HkModDefNode = HkModuleDef NodeInfo
data HkModuleDef a
  = HkModuleDef (HkDottedIdent a) [HkExtStmt a] a
    deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})


-- | Hawk External Statement
--
type HkExtStmtNode = HkExtStmt NodeInfo
data HkExtStmt a
  -- Import statment, may be qualified or unqualified
  = HkExtImport     (HkImportItems a) a
  | HkExtImportQual (HkImportItems a) a
  
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
    
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})


-- | Hawk Import Items
--
-- Import items in hawk are a list of dotted identifiers to some module or
-- external function, record, or variable.
-- 
-- Import items are provided an ident that can overrides their current qualified alias.
-- This can be useful for shorthanded qualified names. An empty alias override will be ignored.
data HkImportItems a
  = HkImportItems [HkDottedIdent a] HkIdent a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

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
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

-- | Hawk Type
--
-- A type in Hawk can be primitive type, reference types, a record type, or even a type signature.
type HkTypeNode = HkType NodeInfo
data HkType a
  = HkTyPrimType    (HkPrimType a) a
  | HkTyRefType     (HkRefType a) a
  | HkTyRecordType  (HkRecordType a) a
  | HkTyTypeSig     (HkTypeSig a) a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

-- | Hawk Primitive Type
--
-- The primitive types in Hawk. 
type HkPrimTypeNode = HkPrimType NodeInfo
data HkPrimType a
  = HkUnitTy a
  | HkBitTy a
  | HkI16Ty a
  | HkI32Ty a
  | HkI64Ty a
  | HkF32Ty a
  | HkF64Ty a
  | HkCharTy a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

-- | Hawk Reference Type
--
-- A reference type is an address to some data. This include pointers, arrays, and type variables.
type HkRefTypeNode = HkRefType NodeInfo
data HkRefType a
  = HkRefType (HkType a) a
  | HkArrayType (HkType a) a
  | HkTypeVariable (HkIdent a) a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

-- | Hawk Record Type
--
-- A record type is a reference to a record defined in program. Records should be
-- constructed if a record is defined with type parameters.
type HkRecordTypeNode = HkRecordType NodeInfo
data HkRecordType a
  = HkRecordType  (HkIdent a) a
  | HkRecordCons  (HkIdent a) [HkType a] a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})


-- | Hawk Type Context
--
-- A type context provides type class restrictions to type variables, which allows a type
-- variable to support multiple interfaces.
type HkTypeContextNode = HkTypeContext NodeInfo
data HkTypeContext a
  = HkTypeContext [HkClassCons a] a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

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
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

-- | Hawk Function Declaration
--
-- A function declaration is the header of a function, containing the name, args, and type signature of a function
type HkFnDecNode = HkFnDec NodeInfo
data HkFnDec a
  = HkFnDec
    { fn_name         :: (HkIdent a)
    , fn_args         :: [HkIdent a]
    , fn_typesig      :: HkTypeSig a
    , fn_annot        :: a
    }
    deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

  
-- | Hawk Function definition
--
-- A function definition consists of a function declarations along with a block that serves as the body.
type HkFnDefNode = HkFnDef NodeInfo
data HkFnDef a
  = HkFnDef (HkFnDec a) (HkBlock a) a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})


-- | Hawk Visibility Tag
--
-- A visibility tag is used to declare the visibility of a function, variable, or record.
type HkVisibilityTagNode = HkVisibilityTag NodeInfo
data HkVisibilityTag a
  = HkPublic a
  | HkPrivate a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

-- | Hawk Binding Declaration
--
-- A binding declaration declares a binding (variable or value) with a name and a type
type HkBindingDecNode = HkBindingDec NodeInfo
data HkBindingDec a
  = HkBindingDec
  { binding_name    :: HkIdent a
  , binding_typesig :: [HkIdent a]
  , binding_annot   :: a
  }
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})
  


-- | Hawk Binding Definition
--
-- A binding declaration is used to bind an identifier to an expression.
type HkBindingDefNode = HkBindingDef NodeInfo
data HkBindingDef a
  = HkBindingDef (HkBindingDef a) (HkExpr a) a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})


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
  = HkValDec (HkBindingDec a) a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

-- | Hawk Value Definiton
--
-- A hawk value represents an immutable binding (i.e. cannot be changed).
--
-- A hawk value definition is a contains a binding definition.
type HkValDefNode = HkValDef NodeInfo
data HkValDef a
  = HkValDef (HkBindingDef a) a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

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
  = HkVarDec (HkBindingDec a) a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})
  
-- | Hawk Variable Definition
--
-- A variable represents a mutable binding (i.e. can be changed).
--
-- A hawk variable definition contains a binding definition.
--
type HkVarDefNode = HkVarDef NodeInfo
data HkVarDef a
  = HkVarDef (HkBindingDef a) a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})
  
  
-- | Hawk Record Definition
--
-- Records in hawk contain variables and values.
type HkRecordNode = HkRecord NodeInfo
data HkRecord a
  = HkRecord
    { rec_name  :: Ident a
    , rec_mems  :: [HkRecordMember a]
    , rec_annot :: a
    }

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
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

-- | Hawk Union Definition
--
-- Unions in hawk are classified as tagged unions. Each element of these unions
-- have a tag, which are enormously helpful for typesafety and pattern matching.
-- It's a minor, minor overhead, and they perform as well as tagless unions.
type HkUnionNode = HkUnion NodeInfo
data HkUnion a
  = HkUnion
  { union_name    :: HkIdent
  , union_tyvars  :: [HkIdent]
  , union_ctx     :: HkTypeContext
  , union_elems   :: [HkUnionElement a]
  , union_annot   :: a
  }
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})
  
-- Element belonging to a union consists of a tag and a list of types.  
type HkUnionElementNode = HkUnionElement NodeInfo
data HkUnionElement a
  = HkUnionElement (HkIdent a) [HkType a] a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

-- | Hawk Statement Block
--
-- A block is a list of statements, which are internal to functions.
type HkBlockNode = HkBlock NodeInfo 
data HkBlock a
  = HkBlock [HkBlockStmt a] a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})

type HkBlockStmtNode = HkBlockStmt NodeInfo
data HkBlockStmt a
  = HkBlkStmt (HkBlockStmt a) a
  | IfThen (HkExpr a) (HkBlock a) a
  | IfThenElse (HkExpr a) (HkBlock a) (HkBlock a) a
  | IfThenElif (HkExpr a) (HkBlock a) (HkExpr a) (HkBlock a) a
  | While (HkExpr a) (HkBlock a) a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})
  
  
type HkExprNode = HkExpr NodeInfo
data HkExpr a
  = HkExprAdd (HkExpr a) (HkExpr a) a
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})