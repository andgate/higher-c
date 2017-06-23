{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Compile.Message
    ( module Language.Hawk.Compile.Message
    , module Language.Hawk.Loader.Message
    , module Language.Hawk.Parse.Message
    , module Language.Hawk.NameCheck.Message
    , module Language.Hawk.TypeCheck.Message
    ) where

import Control.Lens
import Language.Hawk.Loader.Message
import Language.Hawk.Parse.Message
import Language.Hawk.NameCheck.Message
import Language.Hawk.TypeCheck.Message

data HkcMessage
  = HkcLoaderMsg LoaderMessage
  | HkcPsMsg ParseMessage
  | HkcNcMsg NameCheckMessage
  | HkcTcMsg TypeCheckMessage

makeClassyPrisms ''HkcMessage

instance AsLoaderMessage HkcMessage where
  _LoaderMessage = _HkcLoaderMsg

instance AsParseMessage HkcMessage where
  _ParseMessage = _HkcPsMsg

instance AsNameCheckMessage HkcMessage where
  _NameCheckMessage = _HkcNcMsg

instance AsTypeCheckMessage HkcMessage where
  _TypeCheckMessage = _HkcTcMsg