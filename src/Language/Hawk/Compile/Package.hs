{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
  #-}
module Language.Hawk.Compile.Package where

import Control.Lens
import Data.Default.Class
import Data.Text (Text)
import System.FilePath (FilePath)

data Package =
  Package 
    { _pkgName     :: Text
    , _pkgSrcDir   :: FilePath
    }

instance Default Package where
  def = 
    Package "" ""

makeClassy ''Package