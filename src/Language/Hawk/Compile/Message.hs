{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Compile.Message
    ( module Language.Hawk.Compile.Message
    , module Language.Hawk.Load.Message
    , module Language.Hawk.Parse.Message
    , module Language.Hawk.NameCheck.Message
    , module Language.Hawk.TypeCheck.Message
    ) where

import Control.Lens
import Language.Hawk.Load.Message
import Language.Hawk.Parse.Message
import Language.Hawk.NameCheck.Message
import Language.Hawk.TypeCheck.Message

data HkcMessage
  = HkcLoadMsg LoadMsg
  | HkcPsMsg ParseMessage
  | HkcNcMsg NameCheckMessage
  | HkcTcMsg TypeCheckMessage

makeClassyPrisms ''HkcMessage

instance AsLoadMsg HkcMessage where
  _LoadMsg = _HkcLoadMsg

instance AsParseMessage HkcMessage where
  _ParseMessage = _HkcPsMsg

instance AsNameCheckMessage HkcMessage where
  _NameCheckMessage = _HkcNcMsg

instance AsTypeCheckMessage HkcMessage where
  _TypeCheckMessage = _HkcTcMsg