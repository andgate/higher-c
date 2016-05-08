-- Parsec-based Hawk Parser
module Hawk.Language.Parse.Parsec where

import qualified Data.Map as Map
import qualified Data.Traversable as T
import Text.Parsec (char, eof letter many putState (<|>))
import qualified Text.Parsec.Error as Parsec

import qualified Language.Hawk.Syntax.Item as Item
import qualified Language.Hawk.Syntax.Module as Module
import qualified Language.Hawk.Syntax.Module.Name as ModuleName
import qualified Language.Hawk.Compile.Package as Package
import Language.Hawk.Parse.Helpers
import qualified Language.Hawk.Parse.Module as Parse (header)
import qualified Language.Hawk.Parse.Item as Parse (item, infixItem)
import qualified Language.Hawk.Report.Region as R
import qualified Language.Hawk.Report.Error.Syntax as Error
import qualified Language.Hawk.Report.Result as Result
import qualified Language.Hawk.Validate