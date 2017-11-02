{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.TypeCheck.Message where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data TcMsg
  = TypeCheckFail
  | TypeCheckSuccess
  deriving(Show)

makeClassyPrisms ''TcMsg

instance Pretty TcMsg where
    pretty msg =
      undefined
