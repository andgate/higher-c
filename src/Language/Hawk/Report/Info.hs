{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Info where

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
              "File Found: " ++ fp
        
        DirectoryFound fp ->
            Report.simple $
              "Directory found: " ++ fp

        FreshModuleFound mp ->
            Report.simple $
              "Module cached:  " ++ mp
        
        ModulePreserved mp ->
            Report.simple $
              "Module kept: " ++ mp


instance HasFlags Info where
    flagged o i =
      case i of
          FileFound _               -> optFlagInfoFileFound o
          DirectoryFound _          -> optFlagInfoDirectoryFound o
          FreshModuleFound _        -> optFlagInfoFreshModuleFound o
          ModulePreserved _         -> optFlagInfoModulePreserved o


instance HasVerbosity Info where
    verbosity i =
      case i of
          FileFound _               -> 2
          DirectoryFound _          -> 2
          FreshModuleFound _        -> 1
          ModulePreserved _         -> 2