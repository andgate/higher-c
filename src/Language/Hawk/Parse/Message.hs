{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.Parse.Message where

import Control.Lens
import Data.Text (pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>))
import System.FilePath (FilePath)

import qualified Text.PrettyPrint.Leijen.Text as P

data ParseMsg
  = ParseSuccess FilePath
  | UndefinedParseMsg
  deriving(Show)

makeClassyPrisms ''ParseMsg


instance Pretty ParseMsg where
    pretty msg =
      case msg of
        ParseSuccess fp ->
            P.textStrict "Parsed" <+> P.textStrict (pack fp)
    
        UndefinedParseMsg ->
            undefined