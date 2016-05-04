-- | The Core AST
-- This AST is desugared form of the Syntax AST. This AST is
-- meant to be simpler to emit as a target AST, such as LLVM,
-- C, or Javascript.
--
-- This AST is also meant to be optimized after emission.
-- 
-- Finally, this AST may not support all the features that
-- the Syntax AST supports. It should throw an error when
-- an unsupported feature is encountered.
-- Someday, it may support all the features of the Syntax
-- AST, but that is going to take a lot of work.
--
{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module Language.Hawk.Core.AST where

import Data.Generics

-- | Core Module
--
-- Notice that annotations are dropped from core.
-- Emission of the Core AST assumes the syntax AST
-- is completely valid.
data Mod
  = Mod String [Item]
  deriving (Eq, Ord, Show, Data, Typeable)
  
-- | Core Item
-- Notice, at this point, imports and modules are entirely discarded.
-- This may change, if in the future externally referenced types
-- and functions need some sort of declaration.
data Item
  = ItemLinkFn FnDecl
  -- A function item is just a function declaration with an associated
  -- list of statements. Function declarations are not allowed.
  | ItemFn FnDecl [Expr]
  -- A variable item declares a variable and initializes it
  -- with the result of a given function.
  | ItemVar VarDecl Expr
  deriving (Eq, Ord, Show, Data, Typeable)
  
-- | Function Declaration
-- A fairly straighforward declaration.
-- Has a name, return type, and parameters.
data FnDecl
  = FnDecl
    { fn_vis        :: Visibility
    , fn_name       :: String
    , fn_return_ty  :: Type
    , fn_params     :: [Param]
    }
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Variable declaration
-- All variables are currently considered immutable
data VarDecl
  = VarDecl
    { var_vis   :: Visibility
    , var_name  :: String
    , var_type  :: Type
    }
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Visibility
data Visibility
  = Public
  | Private
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Parameter
-- The are function parameters.
data Param
  = Param
  { param_name :: String
  , param_type :: Type
  }
  deriving (Eq, Ord, Show, Data, Typeable)
  

-- Core Expression
data Expr
  = Var String
  | Const Constant
  | Call String [Expr]
  | BinOp BinaryOp Expr Expr
  | UnOp UnaryOp Expr Expr
  deriving (Eq, Ord, Show, Data, Typeable)
  
  
data Constant
  = ConstVoid
  | ConstBool Bool
  | ConstW8   Integer
  | ConstW16  Integer
  | ConstW32  Integer
  | ConstW64  Integer
  | ConstI8   Integer
  | ConstI16  Integer
  | ConstI32  Integer
  | ConstI64  Integer
  | ConstF32  Double
  | ConstF64  Double
  | ConstChar Char
  | ConstString String
  deriving (Eq, Ord, Show, Data, Typeable)
  
data BinaryOp
  = MulOp   -- Multiply
  | DivOp   -- Divide
  | RmdOp   -- Remainder of Division
  | AddOp   -- Addition
  | SubOp   -- Subtraction
  | ShlOp   -- Shift Left
  | ShrOp   -- Shift Right
  | LeOp    -- Lesser Than
  | GrOp    -- Greater Than
  | LeqOp   -- Lesser Than or Equal To
  | GeqOp   -- Greater Than or EqualTo
  | EqOp    -- Equal To
  | NeqOp   -- Not Equal To
  | AndOp   -- Bitwise And
  | XorOp   -- Exclusive Bitwise Or
  | OrOp    -- Inclusive Bitwise Or
  | LndOp   -- Logical And
  | LorOp   -- Logical Or
  deriving (Eq, Ord, Show, Data, Typeable)
  
data UnaryOp
  = PreIncOp    -- Prefix Increment
  | PreDecOp    -- Prefix Decrement
  | PostIncOp   -- Postfix Increment
  | PostDecOp   -- Postfix Decrement
  | AddrOp      -- Address Operator
  | IndOp       -- Indirection Operator
  | PlusOp      -- Prefix plus
  | MinOp       -- Prefix minus
  | CompOp      -- One's Complement
  | NegOp       -- Logical Negation
  deriving (Eq, Ord, Show, Data, Typeable)
  
-- | Types   
data Type
  = PrimTy PrimType -- Only primitive types are currently supported
  deriving (Eq, Ord, Show, Data, Typeable)
  
data PrimType
  = UnitTy
  | BoolTy
  | W8Ty
  | W16Ty
  | W32Ty
  | W64Ty
  | I8Ty
  | I16Ty
  | I32Ty
  | I64Ty
  | F32Ty
  | F64Ty
  | CharTy
  | StringTy
  deriving (Eq, Ord, Show, Data, Typeable)