module Language.Hawk.Analysis.SymbolTable where

data SymbolTable
  = SymbolTable 
  { symtab_scopes :: [Scope]
  , symtab_curr_path :: String
  , 
  }
  
data Scope
  = Scope [String]