{-# LANGUAGE  MultiParamTypeClasses
            , FlexibleInstances
            , FunctionalDependencies
            , LambdaCase
            , OverloadedStrings
  #-}
module Language.Hawk.Target.LLVM.Emit where

import Control.Applicative
import Control.Lens
import Control.Lens.Operators
import Control.Monad.State (MonadState)
import Data.Binary
import Data.ByteString.Short (ShortByteString)
import Data.Int
import Data.Maybe
import Data.Text (Text)
import Data.Word
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Short  as BS
import qualified Data.Map.Strict        as Map
import qualified Data.Text.Encoding     as T
import qualified Foreign                as F

import Language.Hawk.Target.LLVM.Types
import Language.Hawk.Target.LLVM.Codegen

import qualified Language.Hawk.Syntax           as Hk
import qualified Language.Hawk.Syntax.Location  as Hk
import qualified Language.Hawk.Syntax.Operator  as Hk
import qualified Language.Hawk.Syntax.Prim      as Hk

import qualified LLVM.AST                         as AST
import qualified LLVM.AST.Constant                as C
import qualified LLVM.AST.Float                   as F
import qualified LLVM.AST.FloatingPointPredicate  as FP
import qualified LLVM.AST.IntegerPredicate        as IP
import qualified LLVM.AST.Type                    as Ty




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
instance (MonadState s m, HasLLVMState s) => Emittable Hk.CoreMod (m ()) where
  emit (Hk.Mod name items) = do
    llmod . _moduleName .= name
    mapM_ emit items
    
instance (MonadState s m, HasLLVMState s) => Emittable Hk.CoreItem (m ()) where
  emit (Hk.FunItem (Core.FnDecl vis name retty params) body) = do
    bls <- genBlocks params' body
    define retty' name params' bls
    where
      retty' = emit retty
      params' = map emit params
-}
{-  
genParam :: (MonadState s m, HasLLVMState s)
         => AST.Parameter -> m ()
genParam (AST.Parameter ty pname _) = do
  var <- alloca ty
  store var (local pname)
  assignVar pname var
-}


emitDef :: (MonadState s m, HasLLVMState s)
        => Hk.Def -> m ()
emitDef (Hk.Def (Hk.Name name) pats (Hk.ETLit ty exp)) = do
  startBlocks
  emitExp exp
  bls <- endBlocks
  define retty' (t2sbs name) pats' bls
  where
    (pattys, retty) = viewTLit ty
    retty' = emitTypeLit retty
    pats' = map emitPat (zip pats pattys)


emitPat :: (Hk.Pat, Hk.TLit) -> AST.Parameter
emitPat (Hk.Pat name, ty)
    = AST.Parameter ty' name' []
    where ty' = emitTypeLit ty
          name' = AST.Name (t2sbs name)


emitExp :: (MonadState s m, HasLLVMState s)
        => Hk.Exp Hk.Var -> m AST.Operand
emitExp = \case
  Hk.ELit c ->
    setVal $ constOp $ emitLit c
  
  Hk.ETLit ty (Hk.EVar (Hk.Var n)) ->
    getvar (t2sbs n) >>= load (emitTypeLit ty) >>= setVal

  Hk.ETLit ty (Hk.ELet (Hk.Var n) lhs e) -> do
    let ty' = emitTypeLit ty
        n'  = AST.Name (t2sbs n)
    i <- alloca ty'
    val <- emitExp lhs
    store ty' i val
    assignVar n' i
    emitExp e


  Hk.ETLit ty (Hk.EApp (Hk.EVar (Hk.Var n)) arg) -> do
    let ty' = emitTypeLit ty
    arg' <- emitExp arg
    call ty' (externf ty' (AST.Name (t2sbs n))) [arg']

  a@(Hk.EApp _ b) -> do
    let (f, args) = viewApp a
    case f of
      Hk.ETLit ty (Hk.EVar (Hk.Var f')) -> do
        let ty' = emitTypeLit ty
        args' <- mapM emitExp args
        call ty' (externf ty' (AST.Name (t2sbs f'))) args'


  Hk.ETLit ty (Hk.EIf cond tr fl) -> do
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"
    
    -- Entry
    cond <- emitExp cond
    test <- icmp bool IP.EQ (constOp false) cond
    cbr test ifthen ifelse

    -- if.then
    setBlock ifthen
    trval <- emitExp tr
    br ifexit
    ifthen <- fromJust <$> getBlock

    -- if.else
    setBlock ifelse
    flval <- emitExp fl
    br ifexit
    ifelse <- fromJust <$> getBlock

    -- if.exit
    setBlock ifexit
    phi (emitTypeLit ty) [(trval, ifthen), (flval, ifelse)]



  Hk.ELam _ -> error "Lamba expression not lifted"

  Hk.EPrim _ -> error "Prim operation not implemented"

  e -> error ("Unimplemented expression encountered!" ++ show e)



viewApp :: Hk.Exp Hk.Var -> (Hk.Exp Hk.Var, [Hk.Exp Hk.Var])
viewApp = go []
  where
    go xs (Hk.EApp a b) = go (b : xs) a
    go xs f = (f, xs)


viewTLit :: Hk.TLit -> ([Hk.TLit], Hk.TLit)
viewTLit = go []
  where
    go xs (Hk.TLitFun pattys retty) = go pattys retty
    go xs f = (xs, f)


emitLit :: Hk.Lit -> C.Constant
emitLit = \case
  Hk.IntLit v       -> C.Int 64 v
  Hk.FloatLit v     -> C.Float $ F.Double v
  Hk.CharLit v      -> C.Int 8 . toInteger . fromEnum $ v -- Does this work??
  Hk.BoolLit False  -> false
  Hk.BoolLit True   -> true

-------------------------------------------------------------------------------
-- Type Emission
-------------------------------------------------------------------------------

emitTypeLit :: Hk.TLit -> Ty.Type
emitTypeLit = \case
  -- Basic type literals
  Hk.TLitInt    -> Ty.i64
  Hk.TLitFloat  -> Ty.double
  Hk.TLitChar   -> Ty.i8
  Hk.TLitBool   -> bool
  -- More complicated type literals
  Hk.TLitData (Hk.Con n) -> Ty.NamedTypeReference . AST.Name . t2sbs $ n
  Hk.TLitFun args ret     -> Ty.FunctionType (emitTypeLit ret) (map emitTypeLit args) False
  
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