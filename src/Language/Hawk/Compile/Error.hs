{-# LANGUAGE  TemplateHaskell #-}
module Language.Hawk.Compile.Error
    ( module Language.Hawk.Compile.Error
    , module Language.Hawk.Load.Error
    , module Language.Hawk.Lex.Error
    , module Language.Hawk.Parse.Error
    , module Language.Hawk.NameCheck.Error
    , module Language.Hawk.TypeCheck.Error
    ) where


import Control.Lens
import Language.Hawk.Load.Error
import Language.Hawk.Lex.Error
import Language.Hawk.Parse.Error
import Language.Hawk.NameCheck.Error
import Language.Hawk.TypeCheck.Error
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data HkcErr
  = HkcLdErr LdErr
  | HkcLxErr LxErr
  | HkcPsErr PsErr
  | HkcNcErr NcErr
  | HkcTcErr TcErr
  deriving (Show)

makeClassyPrisms ''HkcErr

instance AsLdErr HkcErr where
  _LdErr = _HkcLdErr

instance AsLxErr HkcErr where
  _LxErr = _HkcLxErr

instance AsPsErr HkcErr where
  _PsErr = _HkcPsErr

instance AsNcErr HkcErr where
  _NcErr = _HkcNcErr

instance AsTcErr HkcErr where
  _TcErr = _HkcTcErr



instance Pretty HkcErr where
    pretty (HkcLdErr err) = pretty err
    pretty (HkcLxErr err) = pretty err
    pretty (HkcPsErr err) = pretty err
    pretty (HkcNcErr err) = pretty err
    pretty (HkcTcErr err) = pretty err
