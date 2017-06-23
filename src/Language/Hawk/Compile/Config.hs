{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
  #-}
module Language.Hawk.Compile.Config where

import Control.Lens

import Data.Default.Class
import Data.Text (Text)
import System.FilePath (FilePath)

import Language.Hawk.Compile.Package
import Language.Hawk.Compile.Options


data HkcConfig
  = HkcConfig
    { _hkcArch :: Text
    , _hkcOS   :: Text
    , _hkcRoot  :: FilePath
    , _hkcPkg   :: Package
    , _hkcOpts :: Opts
    }


instance Default HkcConfig where
  def = 
    HkcConfig
      { _hkcArch  = "x86_64"
      , _hkcOS    = "Win10"
      , _hkcRoot  = ""
      , _hkcPkg   = def
      , _hkcOpts  = def
      }

makeClassy ''HkcConfig


instance HasPackage HkcConfig where
  package = hkcPkg