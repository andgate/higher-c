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

data HkcMsg
  = HkcErrMsg HkcErr
  | HkcLoadMsg LoadMsg
  | HkcPsMsg ParseMessage
  | HkcNcMsg NameCheckMessage
  | HkcTcMsg TypeCheckMessage
  deriving(Show)

makeClassyPrisms ''HkcMsg

instance AsHkcErr HkcMsg where
  _HkcErr = _HkcErrMsg

instance AsLoadMsg HkcMsg where
  _LoadMsg = _HkcLoadMsg

instance AsParseMessage HkcMsg where
  _ParseMessage = _HkcPsMsg

instance AsNameCheckMessage HkcMsg where
  _NameCheckMessage = _HkcNcMsg

instance AsTypeCheckMessage HkcMsg where
  _TypeCheckMessage = _HkcTcMsg