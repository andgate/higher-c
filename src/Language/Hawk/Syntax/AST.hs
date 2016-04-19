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
type HkDottedIdentNode = HkDottedIdentifier NodeInfo
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
-- Module definitions contain an list of external (i.e. toplevel) statements.
type HkModDefNode = HkModuleDef NodeInfo
data HkModuleDef a
  = HkModuleDef (DottedIdent a) [HkExtStmt a] a
    deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})


-- | Hawk External Statement
--
type HkExtStmtNode = HkExtStmt NodeInfo
data HkExtStmt a
  = HkExtImport 
    { ext_import_item :: DottedIdent a
    , ext_stmt_annot  :: a
    }
    
  | HkExtFunc (HkDecSpecifier a) (HkExtFunc a) a
    
  deriving (Show, Data, Typeable {-! ,HkNode, Annotated !-})


-- | Hawk Function Declaration
--
-- A function declaration is the header of a function, containing the name, args, and type signature of a function
type HkFnDecNode = HkFnDec NodeInfo
data HkFnDec a
  = HkFnDec
    { fn_name         :: (HkIdent a)
    , fn_args         :: [HkIdent a]
    , fn_typesig      :: [HkIdent a]
    , fn_annot        :: a
    }

  
-- | Hawk Function definition
--
-- A function definition consists of a function declarations along with a block that serves as the body.
type HkFnDefNode = HkFnDef NodeInfo
data HkFnDef a
  = HkFnDef (HkFnDec a) (HkBlock a) a


-- | Hawk Statement Block
--
-- A block is a list of statements, which are internal to functions.
type HkBlockNode = HkBlock NodeInfo 
type HkBlock a = [HkStmt a] a


type HkDecSpecifierNode = HkDecSpecifier NodeInfo
data HkDecSpecifier a
  = HkPublic a
  | HkPrivate a
  | HkExtern a