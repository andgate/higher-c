{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.Load.Message where

import Control.Lens
import Data.Text (pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>))
import System.FilePath (FilePath)

import qualified Text.PrettyPrint.Leijen.Text as P

data LdMsg
  = FileFound FilePath
  | UndefinedLoadMsg
  deriving(Show)

makeClassyPrisms ''LdMsg


instance Pretty LdMsg where
    pretty err =
      case err of
        FileFound fp ->
            P.textStrict "File found:" <+> P.textStrict (pack fp)
    
        UndefinedLoadMsg ->
            undefined
