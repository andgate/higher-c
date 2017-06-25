{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.TypeCheck.Message where

import Control.Lens
import Text.PrettyPrint.Leijen.Text (Pretty(..))

data TypeCheckMessage
  = TypeCheckFail
  | TypeCheckSuccess
  deriving(Show)

makeClassyPrisms ''TypeCheckMessage

instance Pretty TypeCheckMessage where
    pretty msg =
      undefined