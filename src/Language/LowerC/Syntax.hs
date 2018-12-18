{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.LowerC.Syntax where

import Data.Text (Text, unpack)
import Data.Word

import Language.HigherC.Syntax.Extra.Location
import qualified Language.LowerC.Syntax.Extra.Primitive as Prim

data Name
  = Name Loc Text
  | Builtin  Text


nameText :: Name -> Text
nameText = \case
  Name _  txt -> txt
  Builtin txt -> txt

nameStr :: Name -> String
nameStr = \case
  Name _  txt -> unpack txt
  Builtin txt -> unpack txt

data Object
  = Object Name [Defn]

data Defn
  = DGlobal  Global
  | DFunc    Func
  | DExtern  Extern

data Global
  = Global Name Type (Prim.Value Type)

data Func
  = Func FuncDecl [Stmt]

-- has var args lol
data FuncDecl
  = FuncDecl Name [Parameter] Type

data Extern
  = Extern [ExternSpec] Name [Parameter] Type

data ExternSpec
  = ExternVarArg
  | ExternSomeSpec
  deriving (Eq, Ord)

isVarArg :: [ExternSpec] -> Bool
isVarArg = any (== ExternVarArg)

data Parameter
  = Parameter Name Type

data Stmt
  = SNop
  | SExp Exp
  | SLet Type Name Exp
  | SReturn (Maybe Exp)
  | SMatch Exp Type [Case]

data Case
  = Case { ctor_id :: Integer -- Name of the constructor tag to match
         , vars :: [(Integer, Name, Type)] -- Each match can bring in some variables
         , body :: Stmt
         }

data Exp
  = EVar Type Name
  | EAssign Type Name Exp
  
  | EPtr Type Exp
  | EVal (Prim.Value Type)
  | EInstr Instr
  
  | ECall Type Name [Exp]
  | ECon Name [Exp]


data Instr
  = IAdd Exp Exp
  | ISub Exp Exp
  | IMul Exp Exp
  | FAdd Exp Exp
  | FSub Exp Exp
  | FMul Exp Exp
  | FDiv Exp Exp

data Type
  = TCon Name
  | TFun [Type] Type
  | TFunVarArg [Type] Type
  | TVoid
  | TBool
  | TInt Integer
  | TFp  Integer
  | TChar
  | TString
  | TPtr Type
  | TArray Type
  | TArraySized Type Integer


class HasType a where
  typeOf :: a -> Type

instance HasType (Prim.Value Type) where
  typeOf = \case
    Prim.VNull ty -> ty
    Prim.VBool _ -> TBool
    Prim.VInt bits _ -> TInt bits
    Prim.VFp bits _ -> TFp bits
    Prim.VChar _ -> TChar
    Prim.VArray ty vs -> TArraySized ty (toInteger $ length vs)
    Prim.VString cs -> TArraySized (TInt 8) (toInteger $ length cs)
    Prim.VInstr ty _ -> ty


isVoid :: Type -> Bool
isVoid TVoid = True
isVoid     _ = False


data TypeDefn
  = TypeDefn
    { type_name    :: Name
    , type_bytes   :: Integer
    , type_structs :: [TypeStruct]
    }

data TypeStruct
  = TypeStruct
    { struct_name :: Name
    , struct_members :: [Type]
    }
