{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Info where

import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Region (Region, Position)
import Language.Hawk.Report.Report (Report(..), Reportable(..))
import System.FilePath (FilePath)

import qualified Language.Hawk.Report.Region as R
import qualified Language.Hawk.Report.Report as Report

data Info
  = FileFound FilePath


instance Reportable Info where
    toReport info =
      case info of
        FileFound fp ->
            Report.report
              "FileFound"
              Nothing
              "File was found."
              ( reflowParagraph $
                  "Not sure this is really needed."
              )
