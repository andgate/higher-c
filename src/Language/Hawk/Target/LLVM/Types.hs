{-# LANGUAGE TemplateHaskell
  #-}
module Language.Hawk.Target.LLVM.Types where

import Control.Lens
import Data.ByteString.Short (ShortByteString)
import Data.Monoid
import Data.Word
import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.DataLayout

import qualified Data.ByteString.Short as BS
import qualified Data.Map.Strict as Map

-------------------------------------------------------------------------------
-- NameGen
type Names = Map.Map ShortByteString Word8

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just i  -> (nm <> BS.pack [i], Map.insert nm (i+1) ns)

-------------------------------------------------------------------------------
-- Symbol Table
type SymbolTable = [(ShortByteString, Operand)]


-------------------------------------------------------------------------------
-- LLVM State
data LLVMState
  = LLVMState 
    { _llmod          :: Module                   -- The module to edit
    , _llCurrBlock    :: Maybe Name               -- Name of the active block to append to
    , _llBlocks       :: Map.Map Name BlockState  -- Blocks for function
    , _llSymtab       :: SymbolTable              -- Function scope symbol table
    , _llBlockCount   :: Int                      -- Count of basic blocks
    , _llCount        :: Word                     -- Count of unnamed instructions
    , _llNames        :: Names                    -- Name Supply
    } deriving (Show)


-------------------------------------------------------------------------------
-- Block State
data BlockState
  = BlockState
    { _blkIndex   :: Int                            -- Block index
    , _blkStack   :: [Named Instruction]            -- Stack of instructions
    , _blkVal     :: Maybe (Operand)                -- Store a temporary result here
    , _blkTerm    :: Maybe (Named Terminator)       -- Block terminator
    } deriving (Show)


-------------------------------------------------------------------------------
-- Lenses
makeLenses ''BlockState
makeClassy ''LLVMState

-- Extra lenses, for LLVM.AST
_moduleName :: Lens' Module ShortByteString
_moduleName f m = fmap (\y -> m{moduleName = y}) (f . moduleName $ m)
{-# INLINE _moduleName #-}

_moduleSourceFileName :: Lens' Module ShortByteString
_moduleSourceFileName f m = fmap (\y -> m{moduleSourceFileName = y}) (f . moduleSourceFileName $ m)
{-# INLINE _moduleSourceFileName #-}

_moduleDataLayout :: Lens' Module (Maybe DataLayout)
_moduleDataLayout f m = fmap (\y -> m{moduleDataLayout = y}) (f . moduleDataLayout $ m)
{-# INLINE _moduleDataLayout #-}

_moduleTargetTriple :: Lens' Module (Maybe ShortByteString)
_moduleTargetTriple f m = fmap (\y -> m{moduleTargetTriple = y}) (f . moduleTargetTriple $ m)
{-# INLINE _moduleTargetTriple #-}

_moduleDefinitions :: Lens' Module [Definition]
_moduleDefinitions f m = fmap (\y -> m{moduleDefinitions = y}) (f . moduleDefinitions $ m)
{-# INLINE _moduleDefinitions #-}