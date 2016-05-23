module Language.Hawk.Report.Error.Syntax where

import qualified Data.List as List
import qualified Data.Set as Set
import Text.PrettyPrint.ANSI.Leijen (dullyellow, hsep, text)

import qualified Language.Hawk.Report.Error.Helpers as Help
import qualified Language.Hawk.Report.Report as Report

-- -----------------------------------------------------------------------------
-- Syntax Errors
data Error
  = BadFunctionName Int
  | BadPattern String
  
  | CommentOnNothing
  
  | SettingsOnNormalModule
  
  | InfixDuplicate String
  | TypeWithoutDefinition String
  
  | DuplicateArgument String String
  | DuplicateFieldName String
  | DuplicateValueDeclaration String
  | DuplicateTypeDeclaration String
  | DuplicateDefinition String
  | UnboundTypeVarsInUnion String [String] [String]
  | UnboundTypeVarsInAlias String [String] [String]
  | UnusedTypeVarsInAlias String [String] [String]
  | MessyTypeVarsInAlias String [String] [String] [String]