{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.HigherC.Backend where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Data.ByteString.Short (ShortByteString, toShort)

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Name
import LLVM.AST.Type
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import qualified Language.HigherC.Syntax.Core as C

compileObject :: C.Object -> Module
compileObject (C.Object n defns)
  = buildModule (name2sbs n) (compileDefns defns)

-- There is an issue with circular definitions.
-- This should first generate declarations,
-- probably in a topographical order,
-- and then generate definitions.
compileDefns :: [C.Defn] -> ModuleBuilder ()
compileDefns = mapM_ compileDefn

compileDefn :: C.Defn -> ModuleBuilder ()
compileDefn = \case
  C.DGlobal global -> compileGlobal
  C.DFunc   func   -> compileFunc func
  C.DExtern ext    -> compileExtern ext >> return ()


compileGlobal :: C.GlobalDefn -> ModuleBuilder Operand
compileGlobal (C.GlobalDefn name ty cexp)
  = global name' ty' cexp'
  where
    name' = name2name name
    ty' = transType ty
    cexp' = genConstExpr cexp

compileFunc :: C.FuncDefn -> ModuleBuilder ()
compileFunc (C.FuncDefn (C.FuncDecl n params ty) block)
  = undefined


compileExtern :: C.ExternDefn -> ModuleBuilder Operand
compileExtern (C.ExternDefn name params ty)
  = extern name' params' (transType ty)
  where
    name' = name2name name
    params' = [transType ty | (C.Parameter name ty) <- params]


-- Might need to store types in a map of some sort
-- for later retrieval
genTypeDefn :: C.TypeDefn -> ModuleBuilder Type
genTypeDefn (C.TypeDefn n tys)
  = typedef (name2name n) (transType <$> tys)

genConstExpr :: C.ConstExp -> Constant
genConstExpr = \case
  CEVar n -> undefined


transType :: C.Type -> Type
transType = \case
  C.TCon (C.Name _ "Void") -> void

  -- Type Primitives
  C.TCon (C.Name _ "I1")   -> i1
  C.TCon (C.Name _ "I8")   -> i8
  C.TCon (C.Name _ "I16")  -> i16
  C.TCon (C.Name _ "I32")  -> i32
  C.TCon (C.Name _ "I64")  -> i64
  C.TCon (C.Name _ "I128") -> i128

  C.TCon (C.Name _ "Fp16")  -> half
  C.TCon (C.Name _ "Fp32")  -> float
  C.TCon (C.Name _ "Fp64")  -> double
  C.TCon (C.Name _ "Fp128") -> fp128

  -- Needs to look up the structure to get the type
  C.TCon n -> undefined -- structured type!

  C.TPtr   ty -> ptr (transType ty)
  C.TArray ty -> ptr (transType ty)

  C.TArraySized ty i -> ArrayType (fromIntegral i) (transType ty)



name2name :: C.Name -> Name
name2name = mkName . nameStr

name2sbs :: C.Name -> ShortByteString
name2sbs = text2sbs . C.nameText

text2sbs :: Text -> ShortByteString
text2sbs = toShort . encodeUtf8
