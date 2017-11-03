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
import Language.Hawk.Parse.Lex.Error
import Language.Hawk.NameCheck.Error
import Language.Hawk.TypeCheck.Error
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data HkcErr
  = HkcLdErr LdErr
  | HkcPsErr PsErr
  | HkcLexErr LexErr
  | HkcNcErr NcErr
  | HkcTcErr TcErr
  deriving (Show)

makeClassyPrisms ''HkcErr

instance AsLdErr HkcErr where
  _LdErr = _HkcLdErr

instance AsPsErr HkcErr where
  _PsErr = _HkcPsErr

instance AsLexErr HkcErr where
  _LexErr = _HkcLexErr

instance AsNcErr HkcErr where
  _NcErr = _HkcNcErr

instance AsTcErr HkcErr where
  _TcErr = _HkcTcErr



instance Pretty HkcErr where
    pretty (HkcLdErr err) = pretty err
    pretty (HkcPsErr err) = pretty err
    pretty (HkcNcErr err) = pretty err
    pretty (HkcTcErr err) = pretty err
