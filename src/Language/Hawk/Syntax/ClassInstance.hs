module Language.Hawk.Syntax.ClassInstance where

import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.ExpressionDefinition as ED
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDefinition as TD


type Source = 
  ClassInst N.Source E.Source T.Source
  
type Valid = 
  ClassInst N.Valid E.Valid T.Valid

type Typed = 
  ClassInst N.Typed E.Typed T.Typed

type ClassInst n e t =
    TD.TypeDef n t [ED.ExprDef n e t]
    
    
mkClassInst :: QT.Context n -> n -> [n] -> [ED.ExprDef n e t] -> ClassInst n e t
mkClassInst ctx n vs fs =
  TD.TypeDef ctx n vs fs