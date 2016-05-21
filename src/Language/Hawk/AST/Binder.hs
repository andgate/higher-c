module Language.Hawk.AST.Binder where

data Binder n t e a 
  = Binder (BindingMode a) n t e a
  
data BindingMode a
  = ByRef (Mutability a) a
  | ByVal (Mutability a) a
  
data Mutability a
  = Mutable a
  | Immutable a