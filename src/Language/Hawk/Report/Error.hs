{-# LANGUAGE OverloadedStrings #-}
module Language.Hawk.Report.Error where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json


import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Error.Syntax as Syntax
import qualified Language.Hawk.Report.Report as Report

data Error
  = Syntax Syntax.Error