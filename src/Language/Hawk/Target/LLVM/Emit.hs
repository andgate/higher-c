{-# LANGUAGE  MultiParamTypeClasses
            , FlexibleInstances
            , FunctionalDependencies
  #-}
module Language.Hawk.Target.LLVM.Emit where

import Control.Applicative
import Control.Lens
import Control.Lens.Operators
import Control.Monad.State (MonadState)
import Data.Binary
import Data.ByteString.Short (ShortByteString)
import Data.Int
import Data.Text (Text)
import Data.Word
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Short  as BS
import qualified Data.Map.Strict        as Map
import qualified Data.Text.Encoding     as T
import qualified Foreign                as F

import Language.Hawk.Target.LLVM.Types
import Language.Hawk.Target.LLVM.Codegen

import qualified Language.Hawk.Syntax           as Syn
import qualified Language.Hawk.Syntax.Location  as Syn
import qualified Language.Hawk.Syntax.Operator  as Syn
import qualified Language.Hawk.Syntax.Prim      as Syn

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Type as Ty




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

-- I'll work out top-level generation for later
{- 
instance (MonadState s m, HasLLVMState s) => Emittable Syn.CoreMod (m ()) where
  emit (Syn.Mod name items) = do
    llmod . _moduleName .= name
    mapM_ emit items
    
instance (MonadState s m, HasLLVMState s) => Emittable Syn.CoreItem (m ()) where
  emit (Syn.FunItem (Core.FnDecl vis name retty params) body) = do
    bls <- genBlocks params' body
    define retty' name params' bls
    where
      retty' = emit retty
      params' = map emit params
-}


emitFun :: (MonadState s m, HasLLVMState s)
        => Syn.CoreFun -> m ()
emitFun (Syn.Fun (Syn.Name name _) params body retty) = do
  bls <- emitBody body
  define retty' (t2sbs name) params' bls
  where
    retty' = emitTypeLit retty
    params' = map emit params


emitBody :: (MonadState s m, HasLLVMState s)
         => Syn.CoreBody -> m [AST.BasicBlock]
emitBody body = do
  startBlocks
  -- allocate parameters (probably uneccessary)
  -- mapM genParam params
  emit body
  endBlocks

{-  
genParam :: (MonadState s m, HasLLVMState s)
         => AST.Parameter -> m ()
genParam (AST.Parameter ty pname _) = do
  var <- alloca ty
  store var (local pname)
  assignVar pname var
-}

emitFunParam :: Syn.CoreFunParam -> AST.Parameter
emitFunParam (Syn.FunParam name ty)
    = AST.Parameter ty' name' []
    where ty' = emit ty
          name' = AST.Name (t2sbs name)

emitExp :: (MonadState s m, HasLLVMState s)
        => Syn.CoreExp -> m ()
emitExp (Syn.EVar x (Syn.Name n _))
  = getvar (t2sbs n) >>= load >>= setVal
  
emitExp (Syn.ELit _ c)
  = setVal $ constOp $ emit c
  
emit _ = error "Codegen Error: Expression emission not implemented."


emitLit :: Syn.CoreLit -> C.Constant
emitLit (Syn.IntLit Syn.TyLitInt8 v)   = C.Int 8 v
emitLit (Syn.IntLit Syn.TyLitInt16 v)  = C.Int 16 v
emitLit (Syn.IntLit Syn.TyLitInt32 v)  = C.Int 32 v
emitLit (Syn.IntLit Syn.TyLitInt64 v)  = C.Int 64 v

emitLit (Syn.FloatLit Syn.TyLitFloat16 v)  = C.Float $ F.Half $ fromFloat v
emitLit (Syn.FloatLit Syn.TyLitFloat32 v)  = C.Float $ F.Single $ realToFrac v
emitLit (Syn.FloatLit Syn.TyLitFloat64 v)  = C.Float $ F.Double v
-- emit (Syn.FloatLit Syn.TyLitFloat128 v)  = C.Float $ F.Quadruple v

emitLit (Syn.ArrayLit t v)   = C.Array (emit t) (emit <$> v)

emitLit (Syn.BoolLit _ False)  = C.Int 1 0
emitLit (Syn.BoolLit _ True)  = C.Int 1 1

emitLit (Syn.Lit _)        = error "Codegen Error: Cannot emit lit extension"

-------------------------------------------------------------------------------
-- Type Emission
-------------------------------------------------------------------------------

emitTypeLit :: Syn.TypeLit -> Ty.Type
emitTypeLit (Syn.TyLitInt Syn.TyLitInt8) = Ty.i8
emitTypeLit (Syn.TyLitInt Syn.TyLitInt16) = Ty.i16
emitTypeLit (Syn.TyLitInt Syn.TyLitInt32) = Ty.i32
emitTypeLit (Syn.TyLitInt Syn.TyLitInt64) = Ty.i64
emitTypeLit (Syn.TyLitInt Syn.TyLitInt128) = Ty.i128

emitTypeLit (Syn.TyLitFloat Syn.TyLitFloat16) = Ty.half
emitTypeLit (Syn.TyLitFloat Syn.TyLitFloat32) = Ty.float
emitTypeLit (Syn.TyLitFloat Syn.TyLitFloat64) = Ty.double

emitTypeLit (Syn.TyLitArray n ty) = Ty.ArrayType (fromIntegral n) (emitTypeLit ty)
emitTypeLit (Syn.TyLitBool) = Ty.i1
  
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

-- Defunct compilation routines, kept for reference

{-
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
-}

t2sbs :: Text -> ShortByteString
t2sbs =  BS.toShort . T.encodeUtf8

fromFloat :: (F.Storable word, F.Storable float) => float -> word
fromFloat float = unsafePerformIO $ F.alloca $ \buf -> do
	F.poke (F.castPtr buf) float
	F.peek buf