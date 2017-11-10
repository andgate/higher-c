{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.KindsCheck.Message where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data KcMsg
  = KindsCheckFail
  | KindsCheckSuccess
  deriving(Show)

makeClassyPrisms ''KcMsg

instance Pretty KcMsg where
    pretty msg =
      undefined
