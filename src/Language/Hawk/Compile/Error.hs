{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.Compile.Error
    ( module Language.Hawk.Compile.Error
    , module Language.Hawk.Load.Error
    , module Language.Hawk.Parse.Error
    , module Language.Hawk.NameCheck.Error
    , module Language.Hawk.TypeCheck.Error
    ) where


import Control.Lens
import Language.Hawk.Load.Error
import Language.Hawk.Parse.Error
import Language.Hawk.NameCheck.Error
import Language.Hawk.TypeCheck.Error

data HkcErr
  = HkcLoadErr LoadErr
  | HkcParseErr ParseErr
  | HkcNameCheckError NameCheckError
  | HkcTypeCheckError TypeCheckError

makeClassyPrisms ''HkcErr

instance AsLoadErr HkcErr where
  _LoadErr = _HkcLoadErr

instance AsParseErr HkcErr where
  _ParseErr = _HkcParseErr

instance AsNameCheckError HkcErr where
  _NameCheckError = _HkcNameCheckError

instance AsTypeCheckError HkcErr where
  _TypeCheckError = _HkcTypeCheckError