{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Error where

import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Region (Region, Position)
import Language.Hawk.Report.Report (Report(..), Reportable(..))
import System.FilePath (FilePath)

import qualified Language.Hawk.Report.Region as R
import qualified Language.Hawk.Report.Report as Report

data Error
  = Parse R.Region
  | BadModuleName FilePath
  deriving Show

instance Reportable Error where
    toReport err =
      case err of
        Parse r ->
            Report.report
              "UNABLE TO PARSE"
              Nothing
              "There was an error parsing."
              ( reflowParagraph
                  "This should be an example of where the problem was."
              )
        BadModuleName fp ->
            Report.report
              "BAD MODULE NAME"
              Nothing
              ("The path \"" ++ fp ++ "\" does not follow the proper naming convention.")
              ( reflowParagraph
                  "Module names must begin with a capital letter and cannot contain\
                  \ any symbols."
              )