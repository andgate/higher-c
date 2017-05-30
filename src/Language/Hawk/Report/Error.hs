{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Error where

import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Region (Region, Position)
import Language.Hawk.Report.Report (Report(..), Reportable(..))
import System.FilePath (FilePath)

import qualified Language.Hawk.Report.Region as R
import qualified Language.Hawk.Report.Report as Report

data Error
  = Parse FilePath R.Region
  | BadModuleName FilePath
  deriving Show

instance Reportable Error where
    toReport err =
      case err of
        Parse fp (R.R (R.P l c) _) ->
            Report.simple $
              "Parse error " ++ fp ++ ":" ++ show l ++ ":" ++ show c
        BadModuleName fp ->
            Report.report
              "BAD MODULE NAME"
              Nothing
              ("The path \"" ++ fp ++ "\" does not follow the proper naming convention.")
              ( reflowParagraph
                  "Module names must begin with a capital letter and cannot contain\
                  \ any symbols."
              )