{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Info where

import Language.Hawk.Compile.Flags
import Language.Hawk.Compile.Options
import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Region (Region, Position)
import Language.Hawk.Report.Report (Report(..), Reportable(..))
import System.FilePath (FilePath)

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
              "Found file: " ++ fp
        
        DirectoryFound fp ->
            Report.simple $
              "Found Directory: " ++ fp

        FreshModuleFound mp ->
            Report.simple $
              "Cached Module: " ++ mp
        
        ModulePreserved mp ->
            Report.simple $
              "PreservedModule: " ++ mp



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