module Language.Hawk.Syntax.TaggedUnion where

import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDefinition as TD


type Source = 
  TaggedUnion N.Source T.Source
  
type Valid = 
  TaggedUnion N.Valid T.Valid

type Typed = 
  TaggedUnion N.Typed T.Typed

type TaggedUnion n t =
    TD.TypeDef n [(n, t)]
    
    
mkTaggedUnion :: n -> [(n,t)] -> TaggedUnion n t
mkTaggedUnion n fs =
  TD.TypeDef QT.emptyCtx n [] fs