{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
  #-}
module Language.Hawk.Load.Error where

import Control.Lens
import Data.Text (pack)
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<+>))
import System.FilePath

import qualified Text.PrettyPrint.Leijen.Text as P

data LdErr
  = FileNotFound FilePath
  | FileInUse FilePath
  | PermissionDenied FilePath
  | UnexpectedLoadErr FilePath
  deriving(Show)

makeClassyPrisms ''LdErr


instance Pretty LdErr where
    pretty err =
      case err of
        FileNotFound fp ->
            P.textStrict "File not found:" <+> P.textStrict (pack fp)
    
        FileInUse fp -> 
            P.textStrict "File in use:" <+> P.textStrict (pack fp)
    
        PermissionDenied fp ->
            P.textStrict "Permission Denied:" <+> P.textStrict (pack fp)
    
        UnexpectedLoadErr fp ->
            P.textStrict "Unexpected:" <+> P.textStrict (pack fp)
