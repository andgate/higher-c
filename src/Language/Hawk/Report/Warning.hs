{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Warning where

import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Priority
import Language.Hawk.Report.Region (Region, Position)
import Language.Hawk.Report.Report (Report(..), Reportable(..))
import System.FilePath (FilePath)

import qualified Language.Hawk.Report.Region as R
import qualified Language.Hawk.Report.Report as Report

data Warning
  = FileIgnored FilePath
  | DirectoryIgnored FilePath
  | SymLinkIgnored FilePath
  deriving Show

instance Reportable Warning where
    toReport warn =
      case warn of
          FileIgnored fp ->
            Report.report
              "Ignored file"
              Nothing
              "The file didn't have a valid module name"
              ( reflowParagraph $
                  "This should be more detailed"
              )

          DirectoryIgnored fp ->
            Report.report
              "Ignored directory"
              Nothing
              "The given file was found to be empty."
              ( reflowParagraph $
                  "This should be more detailed"
              )

          SymLinkIgnored fp ->
            Report.report
              "Ignored file"
              Nothing
              "The file didn't have a valid module name"
              ( reflowParagraph $
                  "This should be more detailed"
              )


instance HasPriority Warning where
    priority warn =
      case warn of
          FileIgnored _ -> None
          DirectoryIgnored _ -> Standard
          SymLinkIgnored _ -> Standard