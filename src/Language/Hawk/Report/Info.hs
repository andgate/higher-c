{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Info where

import Language.Hawk.Compile.Flags
import Language.Hawk.Compile.Options
import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Region (Region, Position)
import Language.Hawk.Report.Report (Report(..), Reportable(..))
import System.FilePath (FilePath)

import Text.PrettyPrint.ANSI.Leijen (pretty, text, (<>), (<+>))

import qualified Language.Hawk.Report.Region as R
import qualified Language.Hawk.Report.Report as Report

data Info
  = FileFound FilePath
  | DirectoryFound FilePath
  | FreshModuleFound String
  | ModulePreserved String
  deriving Show

instance Reportable Info where
    toReport info =
      case info of
        FileFound fp ->
            Report.simple $
              text "Found file: " <> text fp
        
        DirectoryFound fp ->
            Report.simple $
              text "Found Directory: " <> text fp

        FreshModuleFound mp ->
            Report.simple $
              text "Cached Module: " <> text mp
        
        ModulePreserved mp ->
            Report.simple $
              text "PreservedModule: " <> text mp



instance HasFlags Info where
    flag i = 
      case i of
          FileFound _          -> infoFileFoundFlag
          DirectoryFound _     -> infoDirFoundFlag
          FreshModuleFound _   -> infoFreshModuleFoundFlag
          ModulePreserved _    -> infoFreshModuleFoundFlag


instance HasVerbosity Info where
    verbosityThreshold _ = optInfoVerbosity

    verbosity i =
      case i of
          FileFound _               -> 2
          DirectoryFound _          -> 2
          FreshModuleFound _        -> 1
          ModulePreserved _         -> 2