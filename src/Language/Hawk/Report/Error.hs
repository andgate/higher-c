{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Error where

import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Region (Region, Position)
import Language.Hawk.Report.Report (Report(..), Reportable(..))
import System.FilePath (FilePath)

import Text.PrettyPrint.ANSI.Leijen (pretty, text, (<>), (<+>), dullred)

import qualified Data.Text as T
import qualified Language.Hawk.Parse.Lexer.Token as Tk
import qualified Language.Hawk.Report.Region as R
import qualified Language.Hawk.Report.Report as Report

data Error
  = Parse Tk.Token
  | BadModuleName FilePath
  | UndeclaredVariable -- In expression, from item
  | TypeMismatch -- Expected type, Actual type, in expression, from item
  deriving Show

instance Reportable Error where
    toReport err =
      case err of
        Parse (Tk.Token _ t fp (R.R (R.P l c) _)) ->
            Report.simple $ dullred $
              text "Error parsing '" <> text (T.unpack t) <> text "' at " <> text fp <> ":" <> text (show (l+1)) <> ":" <> text (show (c+1))  
        BadModuleName fp ->
            Report.report
              "BAD MODULE NAME"
              Nothing
              ("The path \"" ++ fp ++ "\" does not follow the proper naming convention.")
              ( reflowParagraph
                  "Module names must begin with a capital letter and cannot contain\
                  \ any symbols."
              )