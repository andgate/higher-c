{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
  #-}
module Language.Hawk.Compile.Config where

import Control.Lens

import Data.Default.Class
import Data.Text (Text)
import System.FilePath (FilePath)

data HkcConfig
  = HkcConfig
    { _hkcSrcFiles     :: [FilePath]
    , _hkcOutType      :: HkcOutputType
    , _hkcOutFile      :: FilePath
    , _hkcDumpToks     :: Bool
    , _hkcDumpParsed   :: Bool
    , _hkcDumpNamed    :: Bool
    , _hkcDumpTyped    :: Bool
    , _hkcDumpKinded   :: Bool
    , _hkcDumpLLVM     :: Bool
    }


data HkcOutputType
    = Bin
    | Lib
    deriving(Show, Eq)

makeClassy ''HkcConfig
makeClassyPrisms ''HkcOutputType


instance Default HkcConfig where
  def = HkcConfig 
    { _hkcSrcFiles     = []
    , _hkcOutType      = Bin
    , _hkcOutFile      = ""
    , _hkcDumpToks     = False
    , _hkcDumpParsed   = False
    , _hkcDumpNamed    = False
    , _hkcDumpTyped    = False
    , _hkcDumpKinded   = False
    , _hkcDumpLLVM     = False
    }
