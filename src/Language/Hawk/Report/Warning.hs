{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Warning where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Text.PrettyPrint.ANSI.Leijen (text)

import qualified Language.Hawk.Syntax.ModuleName as ModuleName
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Error.Helpers as Help
import qualified Language.Hawk.Report.Report as Report


data Warning
  = UnusedImport ModuleName.Raw
  | MissingTypeAnnotation String Type.Canonical