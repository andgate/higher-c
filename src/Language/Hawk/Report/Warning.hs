{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Warning where

import Language.Hawk.Compile.Options
import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Region (Region, Position)
import Language.Hawk.Report.Report (Reportable(..))
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


instance HasFlags Warning where
    flagged o w =
      case w of
          FileIgnored _ -> optWarnFileIgnoredFlag o
          DirectoryIgnored _ -> optWarnDirectoryIgnoredFlag o
          SymLinkIgnored _ -> optWarnSymLinkIgnored o


instance HasVerbosity Warning where
    verbosity w =
      case w of
          FileIgnored _ -> 1
          DirectoryIgnored _ -> 1
          SymLinkIgnored _ -> 1