{-# LANGUAGE LambdaCase #-}
module Language.LowerC.Syntax where

import Data.Text (Text, unpack)
import Data.Word

import Language.HigherC.Syntax.Location
import qualified Language.LowerC.Syntax.Primitive as Prim

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

data FuncDecl
  = FuncDecl Name [Parameter] Type

data Extern
  = Extern Name [Parameter] Type

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
  
  | EVal   (Prim.Value Type)
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
  | TVoid
  | TInt Integer
  | TFp  Integer
  | TChar
  | TPtr Type
  | TArray Type
  | TArraySized Type Integer


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
