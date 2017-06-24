{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
  #-}
module Language.Hawk.Compile.Config where

import Control.Lens

import Data.Default.Class
import Data.Text (Text)
import System.FilePath (FilePath)

import Language.Hawk.Compile.Options


data HkcConfig
  = HkcConfig
    { _hkcSrcFiles  :: [FilePath]
    , _hkcOutFile   :: FilePath
    , _hkcProd      :: HkcProduct
    , _hkcExAst     :: [FilePath] -- List of serialized ast files (hkast)
    , _hkcExLib     :: [FilePath] -- List of library files (.dll or .so)
    , _hkcOpts      :: Opts
    }


data HkcProduct
    = Bin
    | Lib
    | Ast
    deriving(Show, Eq)

makeClassy ''HkcConfig
makeClassyPrisms ''HkcProduct