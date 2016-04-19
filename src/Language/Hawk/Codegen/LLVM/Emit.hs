module Language.Hawk.Codegen.LLVM.Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Language.Hawk.Codegen.LLVM.Codegen
import qualified Language.Hawk.Syntax.AST as Hk


one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

llvmCodegen :: Hk.Expr -> LLVM ()

llvmCodegen (Hk.ModuleExpr _ exprs) =
  mapM_ llvmCodegen exprs

llvmCodegen (Hk.FuncExpr name args types body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.Name a))
        assign a var
      cgen body >>= ret

llvmCodegen (Hk.ExternExpr name args types) = do
  external double name fnargs
  where fnargs = toSig args

llvmCodegen exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp

      
-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------


liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return


codegen :: AST.Module -> [Hk.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM llvmCodegen fns
    newast  = runLLVM mod modn
  

write_as_ir :: FilePath -> Hk.Expr -> IO ()
write_as_ir f expr =
  withContext $ \context ->
    liftError $ withModuleFromAST context newast $ \m ->
      liftError $ writeLLVMAssemblyToFile (File f) m
  where
    mod     = emptyModule f
    modn    = llvmCodegen expr
    newast  = runLLVM mod modn


to_ir_string :: Hk.Expr -> IO String
to_ir_string expr =
  withContext $ \context ->
    liftError $ withModuleFromAST context newast $ \m ->
      moduleLLVMAssembly m
  where
    mod     = emptyModule "Root Module"
    modn    = llvmCodegen expr
    newast  = runLLVM mod modn