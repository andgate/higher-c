{-# LANGUAGE OverloadedStrings #-}
module TestModule where

import qualified LLVM.AST as AST
import qualified LLVM.Module as LLVM
import qualified LLVM.Context as LLVM

import Language.LowerC.Transform.Codegen (genObject)
import Language.LowerC.Syntax
import qualified Language.LowerC.Syntax.Extra.Primitive as Prim


writeTestModule :: IO ()
writeTestModule = LLVM.withContext $ \c -> do
  LLVM.withModuleFromAST c testModule (LLVM.writeLLVMAssemblyToFile (LLVM.File "test.ll"))
  
testModule :: AST.Module
testModule = genObject testObj

testObj :: Object
testObj = Object (Builtin "test") testDefns

testDefns :: [Defn]
testDefns = [printfDefn, mainDefn]

printfDefn :: Defn
printfDefn = DExtern $ Extern [ExternVarArg] (Builtin "printf") [Parameter (Builtin "format") (TPtr TChar)] (TInt 32)

mainDefn :: Defn
mainDefn = DFunc $ Func mainDecl mainBody

mainDecl :: FuncDecl
mainDecl = FuncDecl (Builtin "main") [] (TInt 32)

helloFormatStr :: String
helloFormatStr = "Hello World!\n%d\n"

mainBody :: [Stmt]
mainBody =
  [ SLet (TInt 32) (Builtin "x") (EInstr $ IAdd (EVal (Prim.VInt 32 5)) (EVal (Prim.VInt 32 5))) 
  , SLet TString (Builtin "str") (EVal (Prim.VString helloFormatStr))
  , SExp (ECall (TPtr $ TFunVarArg [TPtr TChar] (TInt 32)) (Builtin "printf") [EVar TString (Builtin "str"), EVar (TInt 32) (Builtin "x")])
  , SReturn (Just (EVar (TInt 32) (Builtin "x")))
  ]