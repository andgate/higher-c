module Language.Hawk.Syntax.ClassInstance where

import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.ExpressionDefinition as ED
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDefinition as TD


type Source = 
  ClassInst N.Source E.Source T.Source
  
type Valid = 
  ClassInst N.Valid E.Valid T.Valid

type Typed = 
  ClassInst N.Typed E.Typed T.Typed

type ClassInst n e t =
    TD.TypeDef n [ED.ExprDef n e t]