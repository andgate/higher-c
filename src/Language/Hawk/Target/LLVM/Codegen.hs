{-# LANGUAGE  GeneralizedNewtypeDeriving
            , TemplateHaskell
            , OverloadedStrings
  #-}
module Language.Hawk.Target.LLVM.Codegen where


-- Haskell library imports
import Control.Applicative
import Control.Lens
import Control.Monad.State (StateT, MonadState, execStateT)
import Data.ByteString.Short (ShortByteString)
import Data.Function
import Data.List
import Data.String
import Data.Word

import qualified Data.ByteString.Short  as BS
import qualified Data.Map.Strict        as Map


-- Hawk imports
import Language.Hawk.Target.LLVM.Types


-- LLVM Imports
import LLVM.AST
import LLVM.AST.Global

import qualified LLVM.AST                         as AST
import qualified LLVM.AST.Attribute               as A
import qualified LLVM.AST.CallingConvention       as CC
import qualified LLVM.AST.Constant                as C
import qualified LLVM.AST.FloatingPointPredicate  as FP
import qualified LLVM.AST.Linkage                 as L
import qualified LLVM.AST.Type                    as Ty

-------------------------------------------------------------------------------
-- Monad

newtype LLVM m a = LLVM { unLLVM :: StateT LLVMState m a }
  deriving (Functor, Applicative, Monad, MonadState LLVMState)

emptyModule :: ShortByteString -> AST.Module
emptyModule label = defaultModule { moduleName = label }  
  
emptyCodegen :: LLVMState
emptyCodegen = LLVMState (emptyModule "") Nothing Map.empty [] 1 0 Map.empty

runLLVM :: Monad m => LLVMState -> LLVM m a -> m LLVMState
runLLVM = flip (execStateT . unLLVM)

  
-------------------------------------------------------------------------------
-- Codegen Operations
addDefn :: (MonadState s m, HasLLVMState s)
        => Definition -> m ()
addDefn d =
  llmod . _moduleDefinitions %= (++ [d])

define :: (MonadState s m, HasLLVMState s)
       => Type -> ShortByteString -> [Parameter] -> [BasicBlock] -> m ()
define retty label params body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = (params, False)
  , returnType  = retty
  , basicBlocks = body
  }

external :: (MonadState s m, HasLLVMState s)
         => Type -> ShortByteString -> [Parameter] -> m ()
external retty label params = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = (params, False)
  , returnType  = retty
  , basicBlocks = []
  }  

def_struct :: (MonadState s m, HasLLVMState s)
           => [Type] -> ShortByteString -> m ()
def_struct tys label = addDefn $
  TypeDefinition (Name label) (Just (StructureType False tys))
  
sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (_blkIndex . snd))

startBlocks :: (MonadState s m, HasLLVMState s)
            => m ()
startBlocks = do
  llBlocks .= Map.empty
  setBlock =<< addBlock entryBlockName
  return ()
  
endBlocks :: (MonadState s m, HasLLVMState s)
          => m [BasicBlock]
endBlocks = do
  bls <- use llBlocks
  llBlocks .= Map.empty
  return $ createBlocks bls

createBlocks :: Map.Map Name BlockState -> [BasicBlock]
createBlocks = map makeBlock . sortBlocks . Map.toList

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s _ t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: ShortByteString
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing Nothing

fresh :: (MonadState s m, HasLLVMState s)
      => m Word
fresh = do
  llCount += 1
  use llCount
  
instr :: (MonadState s m, HasLLVMState s)
      => Instruction -> m (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = _blkStack blk
  modifyBlock (blk { _blkStack = i ++ [ref := ins] } )
  return $ local ref

terminator :: (MonadState s m, HasLLVMState s)
           => Named Terminator -> m (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { _blkTerm = Just trm })
  return trm
  
-------------------------------------------------------------------------------
-- Block Stack
entry :: (MonadState s m, HasLLVMState s)
      => m (Maybe Name)
entry = use llCurrBlock

addBlock :: (MonadState s m, HasLLVMState s)
         => ShortByteString -> m Name
addBlock bname = do
  bls <- use llBlocks
  ix <- use llBlockCount
  nms <- use llNames
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  llBlocks .= Map.insert (Name qname) new bls
  llBlockCount .= ix + 1
  llNames .= supply
  return (Name qname)

setBlock :: (MonadState s m, HasLLVMState s)
         => Name -> m Name
setBlock bname = do
  llCurrBlock .= Just bname
  return bname

getBlock :: (MonadState s m, HasLLVMState s)
         => m (Maybe Name)
getBlock = use llCurrBlock

modifyBlock :: (MonadState s m, HasLLVMState s)
            => BlockState -> m ()
modifyBlock new = do
  active <- use llCurrBlock
  case active of
    Just active' -> llBlocks %= (Map.insert active' new)
    Nothing -> error "Codegen error: Attempted to modify current block when no block is selected."

current :: (MonadState s m, HasLLVMState s)
        => m BlockState
current = do
  c <- use llCurrBlock
  case c of
    Just c' -> do blks <- use llBlocks
                  case Map.lookup c' blks of
                    Just x -> return x
                    Nothing -> error $ "No such block: " ++ show c'
    Nothing -> error "Codegen error: Attempted to access current block when no block is selected."
    
-------------------------------------------------------------------------------
-- Symbol Table
assignVar :: (MonadState s m, HasLLVMState s)
       => Name -> Operand -> m ()
assignVar (Name var) x = do
  lcls <- use llSymtab
  llSymtab %= ([(var, x)] ++)
assignVar (UnName _) _ = error "Codegen Error: Assignment with UnName not supported."

getvar :: (MonadState s m, HasLLVMState s)
       => ShortByteString -> m Operand
getvar var = do
  syms <- use llSymtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var
    
getVal :: (MonadState s m, HasLLVMState s)
       => m (Maybe Operand)
getVal = do
  c <- current
  return $ _blkVal c

setVal :: (MonadState s m, HasLLVMState s)
       => Operand -> m ()
setVal op = do
  c <- current
  modifyBlock $ c & blkVal .~ Just op


-------------------------------------------------------------------------------
-- References
local :: Name -> Operand
local = LocalReference Ty.double

global :: Name -> C.Constant
global = C.GlobalReference Ty.double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference Ty.double

-- Arithmetic and Constants
fadd :: (MonadState s m, HasLLVMState s)
     => Operand -> Operand -> m Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: (MonadState s m, HasLLVMState s)
     => Operand -> Operand -> m Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: (MonadState s m, HasLLVMState s)
     => Operand -> Operand -> m Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: (MonadState s m, HasLLVMState s)
     => Operand -> Operand -> m Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

fcmp :: (MonadState s m, HasLLVMState s)
     => FP.FloatingPointPredicate -> Operand -> Operand -> m Operand
fcmp cond a b = instr $ FCmp cond a b []

constOp :: C.Constant -> Operand
constOp = ConstantOperand

uitofp :: (MonadState s m, HasLLVMState s)
       => Type -> Operand -> m Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: (MonadState s m, HasLLVMState s)
     => Operand -> [Operand] -> m Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: (MonadState s m, HasLLVMState s)
       => Type -> m Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: (MonadState s m, HasLLVMState s)
      => Operand -> Operand -> m Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: (MonadState s m, HasLLVMState s)
     => Operand -> m Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Control Flow
br :: (MonadState s m, HasLLVMState s)
   => Name -> m (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: (MonadState s m, HasLLVMState s)
    => Operand -> Name -> Name -> m (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: (MonadState s m, HasLLVMState s)
    => Operand -> m (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []