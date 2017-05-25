{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Info where

import Language.Hawk.Report.Helpers (reflowParagraph)
import Language.Hawk.Report.Priority
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


instance HasPriority Info where
    priority i =
      case i of
          FileFound _       -> None
          DirectoryFound _  -> None