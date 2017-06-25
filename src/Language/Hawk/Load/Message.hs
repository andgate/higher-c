{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.Load.Message where

import Control.Lens
import Data.Text (pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>))
import System.FilePath (FilePath)

import qualified Text.PrettyPrint.Leijen.Text as P

data LoadMsg
  = FileFound FilePath
  | UndefinedLoadMsg
  deriving(Show)

makeClassyPrisms ''LoadMsg


instance Pretty LoadMsg where
    pretty err =
      case err of
        FileFound fp ->
            P.textStrict "File found:" <+> P.textStrict (pack fp)
    
        UndefinedLoadMsg ->
            undefined