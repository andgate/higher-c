{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Warning where

import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Region (Region, Position)
import Language.Hawk.Report.Report (Report(..), Reportable(..))
import System.FilePath (FilePath)

import qualified Language.Hawk.Report.Region as R
import qualified Language.Hawk.Report.Report as Report

data Warning
  = EmptyModule FilePath
  | IgnoredDirectory FilePath

instance Reportable Warning where
    toReport warn =
      case warn of
          EmptyModule fp ->
              Report.report
                "Empty File"
                Nothing
                "The given file was found to be empty."
                ( reflowParagraph $
                    "This should be more detailed"
                )

          IgnoredDirectory fp ->
            Report.report
              "Ignored directory"
              Nothing
              "The given file was found to be empty."
              ( reflowParagraph $
                  "This should be more detailed"
              )

