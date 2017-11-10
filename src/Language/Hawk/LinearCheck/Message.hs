{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.LinearCheck.Message where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data LcMsg
  = LinearCheckFail
  | LinearCheckSuccess
  deriving(Show)

makeClassyPrisms ''LcMsg

instance Pretty LcMsg where
    pretty msg =
      undefined
