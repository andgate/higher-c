{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Error where

import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Region (Region, Position)
import Language.Hawk.Report.Report (Report(..), Reportable(..))
import System.FilePath (FilePath)

import qualified Data.Text as T
import qualified Language.Hawk.Parse.Lexer.Token as Tk
import qualified Language.Hawk.Report.Region as R
import qualified Language.Hawk.Report.Report as Report

data Error
  = Parse Tk.Token
  | BadModuleName FilePath
  deriving Show

instance Reportable Error where
    toReport err =
      case err of
        Parse (Tk.Token _ t fp (R.R (R.P l c) _)) ->
            Report.simple $
              "Error parsing '" ++ T.unpack t ++ "' at " ++ fp ++ ":" ++ show (l+1) ++ ":" ++ show (c+1)  
        BadModuleName fp ->
            Report.report
              "BAD MODULE NAME"
              Nothing
              ("The path \"" ++ fp ++ "\" does not follow the proper naming convention.")
              ( reflowParagraph
                  "Module names must begin with a capital letter and cannot contain\
                  \ any symbols."
              )