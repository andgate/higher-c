{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.Lex.Message where

import Control.Lens

import Text.PrettyPrint.Leijen.Text (Pretty(..))

{-

data LxMsg
  = UnrecognizedToken
  deriving (Show)


makeClassyPrisms ''LxMsg

instance Pretty LxMsg where
    pretty msg =
      undefined
-}