{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Hawk.Target.LLVM.Emit where

import Control.Applicative
import qualified Control.Lens as L
import Control.Lens.Operators
import Control.Monad.Except

import Data.Convertible.Base
import Data.Int
import qualified Data.Map as Map
import Data.Word

import Language.Hawk.Analysis.Utils
import Language.Hawk.Data.Emittable
import Language.Hawk.Target.LLVM.Codegen

import qualified Language.Hawk.Core.AST as Core


import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.Type as Ty


{-
one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

llvmCodegen :: Hk.Expr -> LLVM ()

llvmCodegen (HkFuncExpr name args types body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.Name a))
        llassign a var
      cgen body >>= ret

llvmCodegen (HkExternExpr name args types) = do
  external double name fnargs
  where fnargs = toSig args

llvmCodegen exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp
      

-}

instance Emittable Core.Mod (LLVM ()) where
  emit (Core.Mod name items) = do
    llmod . _moduleName .= name
    mapM_ emit items
    
instance Emittable Core.Item (LLVM ()) where
  emit (Core.ItemFn (Core.FnDecl vis name retty params) body) = do
    define retty' name params' bls
    where
      retty' = emit retty
      params' = map emit params
      bls = error "Function block generation not implemented." 
        {- createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM args $ \a -> do
          var <- alloca double
          store var (local (AST.Name a))
          llassign a var
        cgen body >>= ret
        -}

instance Emittable Core.Param AST.Parameter where
  emit (Core.Param ty name) = AST.Parameter ty' name' []
    where ty' = emit ty
          name' = AST.Name name

-------------------------------------------------------------------------------
-- Type Emission
-------------------------------------------------------------------------------

instance Emittable Core.Type Ty.Type where
  emit (Core.PrimTy p) = emit p
  
instance Emittable Core.PrimType Ty.Type where
  emit Core.UnitTy = Ty.void
  emit Core.BoolTy = Ty.i1
  emit Core.W8Ty = Ty.i8
  emit Core.W16Ty = Ty.i16
  emit Core.W32Ty = Ty.i32
  emit Core.W64Ty = Ty.i64
  emit Core.I8Ty = Ty.i8
  emit Core.I16Ty = Ty.i16
  emit Core.I32Ty = Ty.i32
  emit Core.I64Ty = Ty.i64
  emit Core.F32Ty = Ty.float
  emit Core.F64Ty = Ty.double
  emit Core.CharTy = Ty.i8
  emit Core.StringTy = error "Strings are not supported."
  
-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------
{-
lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

cgen :: Hk.Expr -> Codegen AST.Operand

cgen (Hk.DoExpr exprs) = do
  doblock <- addBlock "do"
  setBlock doblock
  cvals <- mapM cgen exprs
  return zero
  

cgen (Hk.UnaryOpExpr op a) = do
  cgen $ Hk.CallExpr ("unary" ++ op) [a]

cgen (Hk.BinaryOpExpr "=" (Hk.VarExpr var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval

cgen (Hk.BinaryOpExpr op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"


cgen (Hk.ValDecExpr n t e) = do
  rval <- cgen e
  var  <- alloca double
  store var rval
  assign n var
  return var
  
cgen (Hk.VarDecExpr n t e) = do
  rval <- cgen e
  var  <- alloca double
  store var rval
  assign n var
  return var

cgen (Hk.VarExpr x) = getvar x >>= load

cgen (Hk.FloatExpr n) = return $ cons $ C.Float (F.Double n)
cgen (Hk.IntExpr n) = return $ cons $ C.Float (F.Double (fromIntegral n))

cgen (Hk.CallExpr fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs
  
cgen (Hk.ReturnExpr expr) =
  cgen expr
  
cgen expr = error $ "No codegeneration for " ++ show expr

-}

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------


liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return


codegen :: Core.Mod -> IO AST.Module
codegen ast = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = emit ast
    newast  = (runLLVM emptyCodegen modn) ^. llmod
    

write_as_ir :: FilePath -> Core.Mod -> IO ()
write_as_ir f ast =
  withContext $ \context ->
    liftError $ withModuleFromAST context newast $ \m ->
      liftError $ writeLLVMAssemblyToFile (File f) m
  where
    modn    = emit ast
    newast  = (runLLVM emptyCodegen modn) ^. llmod


to_ir_string :: Core.Mod -> IO String
to_ir_string ast =
  withContext $ \context ->
    liftError $ withModuleFromAST context newast $ \m ->
      moduleLLVMAssembly m
  where
    modn    = emit ast
    newast  = (runLLVM emptyCodegen modn) ^. llmod