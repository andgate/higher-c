{-# LANGUAGE  FlexibleInstances
            , TemplateHaskell
  #-}
module Language.Hawk.Compile.Message
    ( module Language.Hawk.Compile.Message
    , module Language.Hawk.Compile.Error
    , module Language.Hawk.Load.Message
    , module Language.Hawk.Lex.Message
    , module Language.Hawk.Parse.Message
    , module Language.Hawk.NameCheck.Message
    , module Language.Hawk.TypeCheck.Message
    , module Language.Hawk.KindsCheck.Message
    , module Language.Hawk.LinearCheck.Message
    ) where

import Control.Lens
import Language.Hawk.Compile.Error
import Language.Hawk.Load.Message
import Language.Hawk.Lex.Message
import Language.Hawk.Parse.Message
import Language.Hawk.NameCheck.Message
import Language.Hawk.TypeCheck.Message
import Language.Hawk.KindsCheck.Message
import Language.Hawk.LinearCheck.Message
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data HkcMsg
  = HkcErrMsg HkcErr
  | HkcLdMsg LdMsg
  | HkcLxMsg LxMsg
  | HkcPsMsg PsMsg
  | HkcNcMsg NcMsg
  | HkcTcMsg TcMsg
  | HkcKcMsg KcMsg
  | HkcLcMsg LcMsg
  deriving(Show)

makeClassyPrisms ''HkcMsg

instance AsHkcErr HkcMsg where
  _HkcErr = _HkcErrMsg

instance AsLdMsg HkcMsg where
  _LdMsg = _HkcLdMsg

instance AsLxMsg HkcMsg where
  _LxMsg = _HkcLxMsg

instance AsPsMsg HkcMsg where
  _PsMsg = _HkcPsMsg

instance AsNcMsg HkcMsg where
  _NcMsg = _HkcNcMsg

instance AsTcMsg HkcMsg where
  _TcMsg = _HkcTcMsg

instance AsKcMsg HkcMsg where
  _KcMsg = _HkcKcMsg

instance AsLcMsg HkcMsg where
  _LcMsg = _HkcLcMsg


instance Pretty HkcMsg where
    pretty (HkcErrMsg msg) = pretty msg
    pretty (HkcLdMsg msg) = pretty msg
    pretty (HkcLxMsg msg) = pretty msg
    pretty (HkcPsMsg msg) = pretty msg
    pretty (HkcNcMsg msg) = pretty msg
    pretty (HkcTcMsg msg) = pretty msg
    pretty (HkcKcMsg msg) = pretty msg
    pretty (HkcLcMsg msg) = pretty msg
