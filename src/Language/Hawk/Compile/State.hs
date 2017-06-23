{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
  #-}
module Language.Hawk.Compile.State
    ( module Language.Hawk.Compile.State
    , module Language.Hawk.Parse.State
    , module Language.Hawk.NameCheck.State
    , module Language.Hawk.TypeCheck.State
    ) where

import Control.Lens
import Data.Text
import Language.Hawk.Compile.Package
import Language.Hawk.Parse.State
import Language.Hawk.NameCheck.State
import Language.Hawk.TypeCheck.State

import qualified Language.Hawk.Cache.Model as Db

data HkcState = 
  HkcState
    { _hkcPkgId          :: Db.PackageId
    , _hkcParseState     :: ParseState
    , _hkcNameCheckState :: NameCheckState
    , _hkcTypeCheckState :: TypeCheckState
    }

makeClassy ''HkcState

instance HasParseState HkcState where
  parseState = hkcParseState

instance HasNameCheckState HkcState where
  nameCheckState = hkcNameCheckState

instance HasTypeCheckState HkcState where
  typeCheckState = hkcTypeCheckState