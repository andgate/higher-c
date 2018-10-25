{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
  #-}
module Language.Hawk.Compile.Config where

import Control.Lens

import Data.Default.Class
import Data.Text (Text)
import System.FilePath (FilePath)


data HkcOutFmt
    = HkcOutBin
    | HkcOutLib
    deriving(Show, Eq)


data HkcConfig
  = HkcConfig
    { _hkcSrcFiles     :: [FilePath]
    , _hkcOutFile      :: FilePath
    , _hkcOutType      :: HkcOutFmt
    , _hkcBuildDir     :: FilePath
    , _hkcInteractive  :: Bool
    
    , _hkcDumpLxPretty :: Bool
    , _hkcDumpLxBin    :: Bool
    , _hkcDumpLxJson   :: Bool
    , _hkcDumpLxYaml   :: Bool

    , _hkcDumpPsPretty :: Bool
    , _hkcDumpPsBin    :: Bool
    , _hkcDumpPsJson   :: Bool
    , _hkcDumpPsYaml   :: Bool

    , _hkcDumpNcPretty :: Bool
    , _hkcDumpNcBin    :: Bool
    , _hkcDumpNcJson   :: Bool
    , _hkcDumpNcYaml   :: Bool

    , _hkcDumpTcPretty :: Bool
    , _hkcDumpTcBin    :: Bool
    , _hkcDumpTcJson   :: Bool
    , _hkcDumpTcYaml   :: Bool

    , _hkcDumpKcPretty :: Bool
    , _hkcDumpKcBin    :: Bool
    , _hkcDumpKcJson   :: Bool
    , _hkcDumpKcYaml   :: Bool

    , _hkcDumpLcPretty :: Bool
    , _hkcDumpLcBin    :: Bool
    , _hkcDumpLcJson   :: Bool
    , _hkcDumpLcYaml   :: Bool
    
    , _hkcDumpLLVM     :: Bool
    }    


makeClassy ''HkcConfig




instance Default HkcConfig where
  def = HkcConfig 
    { _hkcSrcFiles     = []
    , _hkcOutFile      = ""
    , _hkcOutType      = HkcOutBin
    , _hkcBuildDir     = "./.hkc/"
    , _hkcInteractive  = False

    , _hkcDumpLxPretty = False
    , _hkcDumpLxBin    = False
    , _hkcDumpLxJson   = False
    , _hkcDumpLxYaml   = False

    , _hkcDumpPsPretty = False
    , _hkcDumpPsBin    = False
    , _hkcDumpPsJson   = False
    , _hkcDumpPsYaml   = False

    , _hkcDumpNcPretty = False
    , _hkcDumpNcBin    = False
    , _hkcDumpNcJson   = False
    , _hkcDumpNcYaml   = False
    
    , _hkcDumpTcPretty = False
    , _hkcDumpTcBin    = False
    , _hkcDumpTcJson   = False
    , _hkcDumpTcYaml   = False

    , _hkcDumpKcPretty = False
    , _hkcDumpKcBin    = False
    , _hkcDumpKcJson   = False
    , _hkcDumpKcYaml   = False

    , _hkcDumpLcPretty = False
    , _hkcDumpLcBin    = False
    , _hkcDumpLcJson   = False
    , _hkcDumpLcYaml   = False
    
    , _hkcDumpLLVM     = False
    }
