{-# LANGUAGE  FlexibleInstances
            , TemplateHaskell
  #-}
module Language.Hawk.Compile.Message
    ( module Language.Hawk.Compile.Message
    , module Language.Hawk.Compile.Error
    , module Language.Hawk.Load.Message
    , module Language.Hawk.Parse.Message
    , module Language.Hawk.NameCheck.Message
    , module Language.Hawk.TypeCheck.Message
    ) where

import Control.Lens
import Language.Hawk.Compile.Error
import Language.Hawk.Load.Message
import Language.Hawk.Parse.Message
import Language.Hawk.NameCheck.Message
import Language.Hawk.TypeCheck.Message
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data HkcMsg
  = HkcErrMsg HkcErr
  | HkcLdMsg LdMsg
  | HkcPsMsg PsMsg
  | HkcNcMsg NcMsg
  | HkcTcMsg TcMsg
  deriving(Show)

makeClassyPrisms ''HkcMsg

instance AsHkcErr HkcMsg where
  _HkcErr = _HkcErrMsg

instance AsLdMsg HkcMsg where
  _LdMsg = _HkcLdMsg

instance AsPsMsg HkcMsg where
  _PsMsg = _HkcPsMsg

instance AsNcMsg HkcMsg where
  _NcMsg = _HkcNcMsg

instance AsTcMsg HkcMsg where
  _TcMsg = _HkcTcMsg


instance Pretty HkcMsg where
    pretty (HkcErrMsg msg) = pretty msg
    pretty (HkcLdMsg msg) = pretty msg
    pretty (HkcPsMsg msg) = pretty msg
    pretty (HkcNcMsg msg) = pretty msg
    pretty (HkcTcMsg msg) = pretty msg
