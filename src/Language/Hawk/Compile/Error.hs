{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.Compile.Error
    ( module Language.Hawk.Compile.Error
    , module Language.Hawk.Loader.Error
    , module Language.Hawk.Parse.Error
    , module Language.Hawk.NameCheck.Error
    , module Language.Hawk.TypeCheck.Error
    ) where


import Control.Lens
import Language.Hawk.Loader.Error
import Language.Hawk.Parse.Error
import Language.Hawk.NameCheck.Error
import Language.Hawk.TypeCheck.Error

data HkcError
  = HkcLoaderError LoaderError
  | HkcParseError ParseError
  | HkcNameCheckError NameCheckError
  | HkcTypeCheckError TypeCheckError

makeClassyPrisms ''HkcError

instance AsLoaderError HkcError where
  _LoaderError = _HkcLoaderError

instance AsParseError HkcError where
  _ParseError = _HkcParseError

instance AsNameCheckError HkcError where
  _NameCheckError = _HkcNameCheckError

instance AsTypeCheckError HkcError where
  _TypeCheckError = _HkcTypeCheckError