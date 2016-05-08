module Language.Hawk.Compile.Compiler.Type where


data Type
  = Lambda Type Type
  | Var String
  | Type String
  | App Type [Type]
  | Record [(String, Type)] (Maybe Type)