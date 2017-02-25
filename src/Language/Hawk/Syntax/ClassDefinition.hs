module Language.Hawk.Syntax.ClassDefinition where

import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDefinition as TD


type Source = 
  ClassDef N.Source T.Source
  
type Valid = 
  ClassDef N.Valid T.Valid

type Typed = 
  ClassDef N.Typed T.Typed

type ClassDef n t =
    TD.TypeDef n [(n, t)]
    
    
mkClassDef :: n -> [n] -> [(n,t)] -> ClassDef n t
mkClassDef n vs fs =
  TD.TypeDef QT.emptyCtx n vs fs