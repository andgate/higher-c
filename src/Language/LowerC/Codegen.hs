{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Language.LowerC.Codegen where

import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.Functor.Identity
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Data.Map (Map)
import GHC.Float (double2Float)

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Short (ShortByteString, toShort)

import LLVM.AST hiding (function)
import qualified LLVM.AST.Name as IR
import qualified LLVM.AST.Type as IR
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import qualified Language.LowerC.Syntax as LC
import qualified Language.LowerC.Syntax.Primitive as Prim
import qualified LLVM.AST.Constant as C
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.AST.Float as F
import qualified LLVM.IRBuilder.Constant as C


data GlobalScope = GlobalScope
  { global_vars  :: Map String Operand
  , global_types :: Map String Type
  }

data LocalScope = LocalScope
  { local_blocks :: Map String Name
  , local_vars   :: Map String Operand
  }

genObject :: LC.Object -> Module
genObject (LC.Object n defns)
  = buildModule (name2sbs n) (genDefns defns)

-- There is an issue with circular definitions.
-- This should first generate declarations,
-- probably in a topographical order,
-- and then generate definitions.
genDefns :: (MonadFix m, MonadModuleBuilder m) => [LC.Defn] -> m ()
genDefns = mapM_ genDefn

genDefn :: (MonadFix m, MonadModuleBuilder m) => LC.Defn -> m ()
genDefn = \case
  LC.DGlobal global -> void $ genGlobal global
  LC.DFunc   func   -> void $ genFunc func
  LC.DExtern ext    -> void $ genExtern ext


genGlobal :: (MonadFix m, MonadModuleBuilder m) => LC.Global -> m Operand
genGlobal (LC.Global name ty val)
  = global name' ty' val'
  where
    name' = name2name name
    ty' = genType ty
    val' = genValue val


genFunc :: (MonadFix m, MonadModuleBuilder m) => LC.Func -> m Operand
genFunc (LC.Func (LC.FuncDecl n params ty) block)
  = function (name2name n) (genFuncParam <$> params) (genType ty) (genFuncBody block)


genFuncParam :: LC.Parameter -> (IR.Type, ParameterName)
genFuncParam (LC.Parameter n ty)
  = (genType ty, ParameterName (name2sbs n))


genFuncBody :: (MonadFix m, MonadIRBuilder m) => [LC.Stmt] -> [Operand] -> m ()
genFuncBody stmts ops = do
  let genStmt' stmt = genStmt stmt ops
  block
  mapM_ genStmt' stmts

genStmt :: (MonadFix m, MonadIRBuilder m) => LC.Stmt -> [Operand] -> m ()
genStmt stmt ops = case stmt of
  LC.SNop -> return ()
  LC.SExp e -> void $ genExp e
  LC.SLet ty n e -> do
    --genExp e `named` (name2sbs n)
    return ()


  LC.SReturn Nothing -> I.retVoid
  LC.SReturn (Just e) -> do
    e' <- genExp e
    I.ret e'

  LC.SMatch a ty cases -> mdo
    a' <- genExp a
    -- lookup tag on a'
    let tag = undefined
    switchName <- freshName "switch"
    I.switch tag switchName lookupTable
    lookupTable <- mapM (genCaseBody switchName) cases
    return ()


genCaseName :: MonadIRBuilder m => IR.Name -> LC.Case -> m IR.Name
genCaseName n (LC.Case id _ _)
  = freshName ("case" <> integer2sbs id)

genCaseBody :: (MonadFix m, MonadIRBuilder m)
            => IR.Name -> LC.Case -> m (C.Constant, IR.Name)
genCaseBody switchName (LC.Case id vars body) = do
  let (IR.Name switchNameBS) = switchName
      caseNameBS = switchNameBS <> ".case" <> integer2sbs id
  caseName <- block `named` caseNameBS
  genStmt body []
  return (C.Int 32 id, caseName)


genExp :: MonadIRBuilder m => LC.Exp -> m Operand
genExp = \case
  LC.EVar ty n -> I.load (LocalReference (genType ty) (name2name n)) 1
  LC.EAssign ty n e -> do
    e' <- genExp e
    let lref = (LocalReference (genType ty) (name2name n))
    I.store lref 1 e'
    return lref
  
  LC.EVal v -> return $ ConstantOperand $ genValue v
  LC.EInstr i -> genInstr i

  LC.ECall ty n args -> do 
    args' <- mapM genExp args
    let gref = ConstantOperand $ C.GlobalReference (genType ty) (name2name n)
    I.call gref [(arg, []) | arg <- args']

  -- Constructors are tricky, since they are usually
  -- associated with a new or something
  LC.ECon n args  -> undefined 


genExtern :: MonadModuleBuilder m => LC.Extern -> m Operand
genExtern (LC.Extern name params ty)
  = extern name' params' (genType ty)
  where
    name' = name2name name
    params' = [genType ty | (LC.Parameter name ty) <- params]



genTypeDefn :: MonadModuleBuilder m => LC.TypeDefn -> m IR.Type
genTypeDefn LC.TypeDefn{..} =
  case type_structs of

    [] -> typedef (name2name type_name) Nothing

    [(LC.TypeStruct{..})] -> do
      let tys = genType <$> struct_members
          ir_struct = IR.StructureType False tys
          n = mkStructName type_name struct_name
      typedef n (Just ir_struct)

    _ -> do
      mapM_ (genStruct type_name) type_structs
      let arr_size = fromIntegral type_bytes
          ir_struct_arr = IR.ArrayType arr_size IR.i8
          ir_struct = IR.StructureType False [IR.i8, ir_struct_arr]
      typedef (name2name type_name) (Just ir_struct)


genStruct :: MonadModuleBuilder m => LC.Name -> LC.TypeStruct -> m IR.Type
genStruct type_name (LC.TypeStruct{..}) = do
  let tys = genType <$> struct_members
      n = mkStructName type_name struct_name
      ir_struct =  IR.StructureType False ([IR.i8] ++ tys)
  typedef n (Just ir_struct)


mkStructName :: LC.Name -> LC.Name -> IR.Name
mkStructName type_name struct_name
  = mkName (LC.nameStr type_name <> "_" <> LC.nameStr struct_name)

genValue :: Prim.Value LC.Type -> C.Constant
genValue = \case
  Prim.VNull t -> C.Null (genType t)
  Prim.VInt bits i  -> C.Int (fromIntegral bits) i
  
  -- How?
  -- Prim.VFp  16 f   -> C.Float $ F.Half (f2Word16 g) 
  Prim.VFp  32 f   -> C.Float $ F.Single (double2Float f)
  Prim.VFp  64 f   -> C.Float $ F.Double f
  -- PrimV.Fp 32 f  -> C.Float $ F.Quadruple word16 word16??
  
  Prim.VChar c -> genChar c

  Prim.VArray t vals -> C.Array (genType t) (genValue <$> vals)
  Prim.VVector vals  -> C.Vector (genValue <$> vals)
  Prim.VString str   -> C.Array IR.i8 (genChar <$> str)
  Prim.VInstr  i     -> genPrimInstr i


genChar :: Char -> C.Constant
genChar = C.Int 8 . fromIntegral . ord


genInstr :: MonadIRBuilder m => LC.Instr -> m Operand
genInstr = \case
  LC.IAdd a b -> do
    a' <- genExp a
    b' <- genExp b
    I.add a' b'

  LC.ISub a b -> do
    a' <- genExp a
    b' <- genExp b
    I.sub a' b'

  LC.IMul a b -> do
    a' <- genExp a
    b' <- genExp b
    I.mul a' b'

  LC.FAdd a b -> do
    a' <- genExp a
    b' <- genExp b
    I.fadd a' b'

  LC.FSub a b -> do
    a' <- genExp a
    b' <- genExp b
    I.fsub a' b'

  LC.FMul a b -> do
    a' <- genExp a
    b' <- genExp b
    I.fmul a' b'

  LC.FDiv a b -> do
    a' <- genExp a
    b' <- genExp b
    I.fdiv a' b'


genPrimInstr :: Prim.Instruction LC.Type -> C.Constant
genPrimInstr = \case
  Prim.IAdd a b ->
    C.Add False False (genValue a) (genValue b)

  Prim.ISub a b ->
    C.Sub False False (genValue a) (genValue b)

  Prim.IMul a b ->
    C.Mul False False (genValue a) (genValue b)

  Prim.FAdd a b ->
    C.FAdd (genValue a) (genValue b)

  Prim.FSub a b ->
    C.FSub (genValue a) (genValue b)

  Prim.FMul a b ->
    C.FMul (genValue a) (genValue b)

  Prim.FDiv a b ->
    C.FDiv (genValue a) (genValue b)



genType :: LC.Type -> IR.Type
genType = \case
  LC.TVoid -> IR.void

  LC.TFun args ret ->
    IR.FunctionType (genType ret) (genType <$> args) False

  -- Type Primitives
  LC.TInt 1   -> IR.i1
  LC.TInt 8   -> IR.i8
  LC.TInt 16  -> IR.i16
  LC.TInt 32  -> IR.i32
  LC.TInt 64  -> IR.i64
  LC.TInt 128 -> IR.i128

  LC.TFp 16  -> IR.half
  LC.TFp 32  -> IR.float
  LC.TFp 64  -> IR.double
  LC.TFp 128 -> IR.fp128

  LC.TChar   -> IR.i8

  LC.TCon n -> IR.NamedTypeReference (name2name n)

  LC.TPtr   ty -> IR.ptr (genType ty)
  LC.TArray ty -> IR.ptr (genType ty)

  LC.TArraySized ty i -> IR.ArrayType (fromIntegral i) (genType ty)



name2name :: LC.Name -> IR.Name
name2name = mkName . LC.nameStr

name2sbs :: LC.Name -> ShortByteString
name2sbs = text2sbs . LC.nameText

text2sbs :: Text -> ShortByteString
text2sbs = toShort . encodeUtf8

integer2sbs :: Integer -> ShortByteString
integer2sbs = toShort . BS.pack . show
