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
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data HkcErr
  = HkcLoadErr LoadErr
  | HkcParseErr ParseErr
  | HkcNameCheckError NameCheckError
  | HkcTypeCheckError TypeCheckError
  deriving (Show)

makeClassyPrisms ''HkcErr

instance AsLoadErr HkcErr where
  _LoadErr = _HkcLoadErr

instance AsParseErr HkcErr where
  _ParseErr = _HkcParseErr

instance AsNameCheckError HkcErr where
  _NameCheckError = _HkcNameCheckError

instance AsTypeCheckError HkcErr where
  _TypeCheckError = _HkcTypeCheckError


instance Pretty HkcErr where
    pretty (HkcLoadErr err) = pretty err
    pretty (HkcParseErr err) = pretty err
    pretty (HkcNameCheckError err) = pretty err
    pretty (HkcTypeCheckError err) = pretty err