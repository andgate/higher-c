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


data HkcTokFmt
  = TokPretty
  | TokJSON
  | TokYaml
  | TokBin


data HkcAstFmt
  = AstPretty
  | AstJSON
  | AstYAML
  | AstBin
  deriving (Show, Eq)


data HkcConfig
  = HkcConfig
    { _hkcSrcFiles     :: [FilePath]
    , _hkcOutFile      :: FilePath
    , _hkcOutType      :: HkcOutFmt
    , _hkcDumpLx       :: Maybe HkcAstFmt
    , _hkcDumpPs       :: Maybe HkcAstFmt
    , _hkcDumpNc       :: Maybe HkcAstFmt
    , _hkcDumpTc       :: Maybe HkcAstFmt
    , _hkcDumpKc       :: Maybe HkcAstFmt
    , _hkcDumpLLVM     :: Bool
    }    

makeClassy ''HkcConfig




instance Default HkcConfig where
  def = HkcConfig 
    { _hkcSrcFiles     = []
    , _hkcOutFile      = ""
    , _hkcOutType      = HkcOutBin
    , _hkcDumpLx       = Nothing 
    , _hkcDumpPs       = Nothing
    , _hkcDumpNc       = Nothing
    , _hkcDumpTc       = Nothing
    , _hkcDumpKc       = Nothing
    , _hkcDumpLLVM     = False
    }
