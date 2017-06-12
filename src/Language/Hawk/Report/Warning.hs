{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Warning where

import Language.Hawk.Compile.Flags
import Language.Hawk.Compile.Options
import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Region (Region, Position)
import Language.Hawk.Report.Report (Reportable(..))
import System.FilePath (FilePath)


import Text.PrettyPrint.ANSI.Leijen (pretty, text, (<>), (<+>), yellow)

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
            Report.simple $ yellow $
              text "Ignored directory:" <+> text fp

          DirectoryIgnored fp ->
            Report.simple $ yellow $
              text "Ignored directory:" <+> text fp

          SymLinkIgnored fp ->
            Report.simple $ yellow $
              text "Ignored Symbolic Link:" <+> text fp





instance HasFlags Warning where
    flag w = 
      case w of
          FileIgnored _       -> warnFileIgnoredFlag
          DirectoryIgnored _  -> warnDirIgnoredFlag
          SymLinkIgnored _    -> warnSymLinkIgnoredFlag


instance HasVerbosity Warning where
    verbosityThreshold _ = optWarnVerbosity

    verbosity w =
      case w of
          FileIgnored _ -> 1
          DirectoryIgnored _ -> 1
          SymLinkIgnored _ -> 1