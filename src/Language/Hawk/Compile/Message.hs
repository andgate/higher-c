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
  | HkcLoadMsg LoadMsg
  | HkcPsMsg ParseMsg
  | HkcNcMsg NcMsg
  | HkcTcMsg TypeCheckMessage
  deriving(Show)

makeClassyPrisms ''HkcMsg

instance AsHkcErr HkcMsg where
  _HkcErr = _HkcErrMsg

instance AsLoadMsg HkcMsg where
  _LoadMsg = _HkcLoadMsg

instance AsParseMsg HkcMsg where
  _ParseMsg = _HkcPsMsg

instance AsNcMsg HkcMsg where
  _NcMsg = _HkcNcMsg

instance AsTypeCheckMessage HkcMsg where
  _TypeCheckMessage = _HkcTcMsg


instance Pretty HkcMsg where
    pretty (HkcErrMsg msg) = pretty msg
    pretty (HkcLoadMsg msg) = pretty msg
    pretty (HkcPsMsg msg) = pretty msg
    pretty (HkcNcMsg msg) = pretty msg
    pretty (HkcTcMsg msg) = pretty msg
