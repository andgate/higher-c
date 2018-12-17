{-# LANGUAGE OverloadedStrings #-}
module TestModule where

import qualified LLVM.AST as AST
import qualified LLVM.Module as LLVM
import qualified LLVM.Context as LLVM

import Language.LowerC.Codegen (genObject)
import Language.LowerC.Syntax
import qualified Language.LowerC.Syntax.Primitive as Prim


writeTestModule :: IO ()
writeTestModule = LLVM.withContext $ \c -> do
  LLVM.withModuleFromAST c testModule (LLVM.writeLLVMAssemblyToFile (LLVM.File "text.ll"))
  
testModule :: AST.Module
testModule = genObject testObj

testObj :: Object
testObj = Object (Builtin "test") testDefns

testDefns :: [Defn]
testDefns = [printfDefn, mainDefn]

printfDefn :: Defn
printfDefn = DExtern $ Extern (Builtin "printf") [Parameter (Builtin "str") (TPtr TChar)] (TFun [TPtr TChar] TVoid)

mainDefn :: Defn
mainDefn = DFunc $ Func mainDecl mainBody

mainDecl :: FuncDecl
mainDecl = FuncDecl (Builtin "main") [] (TFun [TVoid] (TInt 32))

mainBody :: [Stmt]
mainBody =
  [ SLet (TInt 32) (Builtin "x") (EInstr $ IAdd (EVal (Prim.VInt 32 5)) (EVal (Prim.VInt 32 5))) 
  , SExp (ECall TVoid (Builtin "printf") [(EVal (Prim.VString "Hello World!"))])
  , SReturn (Just (EVar (TInt 32) (Builtin "x")))
  ]