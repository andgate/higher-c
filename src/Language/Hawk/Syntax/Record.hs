module Language.Hawk.Syntax.Record where

import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDefinition as TD


type Source = 
  Record N.Source T.Source
  
type Valid = 
  Record N.Valid T.Valid

type Typed = 
  Record N.Typed T.Typed

type Record n t =
    TD.TypeDef n [(n, t)]