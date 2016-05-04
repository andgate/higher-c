{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Hawk.Codegen.LLVM.Emit where

import Control.Applicative
import qualified Control.Lens as L
import Control.Lens.Operators
import Control.Monad.Except

import Data.Convertible.Base
import Data.Int
import qualified Data.Map as Map
import Data.Word

import Language.Hawk.Analysis.Utils
import Language.Hawk.Codegen.LLVM.Codegen
import Language.Hawk.Syntax.AST


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

class Emit a where
  emit :: a -> LLVM () 

instance Emit (HkTranslUnit a) where
  emit (HkTranslUnit (HkMod path (HkModBlock items _) _)) = do
    llmod . _moduleName .= show path
    mapM_ emit items
    
instance Emit (HkModItem a) where
  emit _ = return ()

instance Emit (HkTypeDef a) where
  emit _ = return ()
  
instance Emit (HkRecDef a) where
  emit r = return ()


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

instance Convertible (HkType a) Ty.Type where
  safeConvert (HkTyPrim p _) = safeConvert p
  safeConvert _ = undefined
  
instance Convertible (HkPrimType a) Ty.Type where
  safeConvert (HkTyUnit _) = Right Ty.void
  safeConvert (HkTyBool _) = Right Ty.i1
  safeConvert (HkTyW8 _) = Right Ty.i8
  safeConvert (HkTyW16 _) = Right Ty.i16
  safeConvert (HkTyW32 _) = Right Ty.i32
  safeConvert (HkTyW64 _) = Right Ty.i64
  safeConvert (HkTyI8 _) = Right Ty.i8
  safeConvert (HkTyI16 _) = Right Ty.i16
  safeConvert (HkTyI32 _) = Right Ty.i32
  safeConvert (HkTyI64 _) = Right Ty.i64
  safeConvert (HkTyF32 _) = Right Ty.float
  safeConvert (HkTyF64 _) = Right Ty.double
  safeConvert (HkTyChar _) = Right Ty.i8
  safeConvert (HkTyString _)
    = Left $ ConvertError
      { convSourceValue = "HkTyString" 
      , convSourceType = "HkPrimType"
      , convDestType = "Type"
      , convErrorMessage = "String types are unsupported."
      }
  
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


codegen :: HkTranslUnit a -> IO AST.Module
codegen ast = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = emit ast
    newast  = (runLLVM emptyCodegen modn) ^. llmod
    

write_as_ir :: FilePath -> HkTranslUnit a -> IO ()
write_as_ir f ast =
  withContext $ \context ->
    liftError $ withModuleFromAST context newast $ \m ->
      liftError $ writeLLVMAssemblyToFile (File f) m
  where
    modn    = emit ast
    newast  = (runLLVM emptyCodegen modn) ^. llmod


to_ir_string :: HkTranslUnit a -> IO String
to_ir_string ast =
  withContext $ \context ->
    liftError $ withModuleFromAST context newast $ \m ->
      moduleLLVMAssembly m
  where
    modn    = emit ast
    newast  = (runLLVM emptyCodegen modn) ^. llmod