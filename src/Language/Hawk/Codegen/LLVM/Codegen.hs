{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Hawk.Codegen.LLVM.Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import qualified Control.Lens as L
import Control.Lens.Operators
import Control.Monad.State
import Control.Applicative

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Type as Ty
import qualified LLVM.General.AST.DataLayout as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP


-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

instance IsString Name where
  fromString = Name . fromString
  

-------------------------------------------------------------------------------
-- Codegen Definitions
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]


data BlockState
  = BlockState
    { _blkIndex   :: Int                            -- Block index
    , _blkStack   :: [Named Instruction]            -- Stack of instructions
    , _blkVal     :: Maybe (Operand)                -- Store a temporary result here
    , _blkTerm    :: Maybe (Named Terminator)       -- Block terminator
    } deriving (Show)


data LLVMState
  = LLVMState 
    { _llmod          :: AST.Module               -- The module to edit
    , _llCurrBlock    :: Name                     -- Name of the active block to append to
    , _llBlocks       :: Map.Map Name BlockState  -- Blocks for function
    , _llSymtab       :: SymbolTable              -- Function scope symbol table
    , _llBlockCount   :: Int                      -- Count of basic blocks
    , _llCount        :: Word                     -- Count of unnamed instructions
    , _llNames        :: Names                    -- Name Supply
    } deriving (Show)

newtype LLVM a = LLVM { unLLVM :: State LLVMState a }
  deriving (Functor, Applicative, Monad, MonadState LLVMState)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }  
  
emptyCodegen :: LLVMState
emptyCodegen = LLVMState (emptyModule "") (Name entryBlockName) Map.empty [] 1 0 Map.empty

runLLVM :: LLVMState -> LLVM a -> LLVMState
runLLVM = flip (execState . unLLVM)


-------------------------------------------------------------------------------
-- Codegen Lenses
-------------------------------------------------------------------------------

-- Lenses have to be written manually, as stack seems to have trouble dumping
-- template haskell errors to console.

-- Block State Lenses
blkIndex :: L.Lens' BlockState Int
blkIndex f (BlockState x1 x2 x3 x4) = fmap (\y -> BlockState y x2 x3 x4) (f x1)
{-# INLINE blkIndex #-}

blkStack :: L.Lens' BlockState [Named Instruction]
blkStack f (BlockState x1 x2 x3 x4) = fmap (\y -> BlockState x1 y x3 x4) (f x2)
{-# INLINE blkStack #-}

blkVal :: L.Lens' BlockState (Maybe (Operand))
blkVal f (BlockState x1 x2 x3 x4) = fmap (\y -> BlockState x1 x2 y x4) (f x3)
{-# INLINE blkVal #-}

blkTerm :: L.Lens' BlockState (Maybe (Named Terminator) )
blkTerm f (BlockState x1 x2 x3 x4) = fmap (\y -> BlockState x1 x2 x3 y) (f x4)
{-# INLINE blkTerm #-}
  
-------------------------------------------------------------------------------  
-- LLVM State lenses
llmod :: L.Lens' LLVMState AST.Module
llmod f (LLVMState x1 x2 x3 x4 x5 x6 x7) = fmap (\y -> LLVMState y x2 x3 x4 x5 x6 x7) (f x1)
{-# INLINE llmod #-}

llCurrBlock :: L.Lens' LLVMState Name
llCurrBlock f (LLVMState x1 x2 x3 x4 x5 x6 x7) = fmap (\y -> LLVMState x1 y x3 x4 x5 x6 x7) (f x2)
{-# INLINE llCurrBlock #-}

llBlocks :: L.Lens' LLVMState (Map.Map Name BlockState)
llBlocks f (LLVMState x1 x2 x3 x4 x5 x6 x7) = fmap (\y -> LLVMState x1 x2 y x4 x5 x6 x7) (f x3)
{-# INLINE llBlocks #-}

llSymtab :: L.Lens' LLVMState SymbolTable
llSymtab f (LLVMState x1 x2 x3 x4 x5 x6 x7) = fmap (\y -> LLVMState x1 x2 x3 y x5 x6 x7) (f x4)
{-# INLINE llSymtab #-}

llBlockCount :: L.Lens' LLVMState Int
llBlockCount f (LLVMState x1 x2 x3 x4 x5 x6 x7) = fmap (\y -> LLVMState x1 x2 x3 x4 y x6 x7) (f x5)
{-# INLINE llBlockCount #-}

llCount :: L.Lens' LLVMState Word
llCount f (LLVMState x1 x2 x3 x4 x5 x6 x7) = fmap (\y -> LLVMState x1 x2 x3 x4 x5 y x7) (f x6)
{-# INLINE llCount #-}

llNames :: L.Lens' LLVMState Names
llNames f (LLVMState x1 x2 x3 x4 x5 x6 x7) = fmap (\y -> LLVMState x1 x2 x3 x4 x5 x6 y) (f x7)
{-# INLINE llNames #-}
  
-- Miscellaneous Lenses
_moduleName :: L.Lens' AST.Module String
_moduleName f (AST.Module x1 x2 x3 x4) = fmap (\y -> AST.Module y x2 x3 x4) (f x1)
{-# INLINE _moduleName #-}

_moduleDataLayout :: L.Lens' AST.Module (Maybe AST.DataLayout)
_moduleDataLayout f (AST.Module x1 x2 x3 x4) = fmap (\y -> AST.Module x1 y x3 x4) (f x2)
{-# INLINE _moduleDataLayout #-}

_moduleTargetTriple :: L.Lens' AST.Module (Maybe String)
_moduleTargetTriple f (AST.Module x1 x2 x3 x4) = fmap (\y -> AST.Module x1 x2 y x4) (f x3)
{-# INLINE _moduleTargetTriple #-}

_moduleDefinitions :: L.Lens' AST.Module [AST.Definition]
_moduleDefinitions f (AST.Module x1 x2 x3 x4) = fmap (\y -> AST.Module x1 x2 x3 y) (f x4)
{-# INLINE _moduleDefinitions #-}
  
-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------
  

addDefn :: Definition -> LLVM ()
addDefn d = do
  mod <- gets _llmod
  let defs = moduleDefinitions mod
      mod' = mod { moduleDefinitions = defs ++ [d] }
  modify $ \s -> s { _llmod = mod' }

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }  

def_struct :: [Type] -> String -> LLVM ()
def_struct tys label = addDefn $
  TypeDefinition (Name label) (Just (StructureType False tys))
  
sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (_blkIndex . snd))

createBlocks :: LLVMState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (_llBlocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s _ t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing Nothing

fresh :: LLVM Word
fresh = do
  i <- gets _llCount
  modify $ \s -> s { _llCount = 1 + i }
  return $ i + 1
  
instr :: Instruction -> LLVM (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = _blkStack blk
  modifyBlock (blk { _blkStack = i ++ [ref := ins] } )
  return $ local ref

terminator :: Named Terminator -> LLVM (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { _blkTerm = Just trm })
  return trm
  
-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: LLVM Name
entry = gets _llCurrBlock

addBlock :: String -> LLVM Name
addBlock bname = do
  bls <- gets _llBlocks
  ix <- gets _llBlockCount
  nms <- gets _llNames
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { _llBlocks = Map.insert (Name qname) new bls
                   , _llBlockCount = ix + 1
                   , _llNames = supply
                   }
  return (Name qname)

setBlock :: Name -> LLVM Name
setBlock bname = do
  modify $ \s -> s { _llCurrBlock = bname }
  return bname

getBlock :: LLVM Name
getBlock = gets _llCurrBlock

modifyBlock :: BlockState -> LLVM ()
modifyBlock new = do
  active <- gets _llCurrBlock
  modify $ \s -> s { _llBlocks = Map.insert active new (_llBlocks s) }

current :: LLVM BlockState
current = do
  c <- gets _llCurrBlock
  blks <- gets _llBlocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c
    
-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> LLVM ()
assign var x = do
  lcls <- gets _llSymtab
  modify $ \s -> s { _llSymtab = [(var, x)] ++ lcls }

getvar :: String -> LLVM Operand
getvar var = do
  syms <- gets _llSymtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var
    
    
-------------------------------------------------------------------------------

-- References
local ::  Name -> Operand
local = LocalReference Ty.double

global ::  Name -> C.Constant
global = C.GlobalReference Ty.double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference Ty.double

-- Arithmetic and Constants
fadd :: Operand -> Operand -> LLVM Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> LLVM Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> LLVM Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> LLVM Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> LLVM Operand
fcmp cond a b = instr $ FCmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> LLVM Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> LLVM Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> LLVM Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> LLVM Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> LLVM Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Control Flow
br :: Name -> LLVM (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> LLVM (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> LLVM (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []