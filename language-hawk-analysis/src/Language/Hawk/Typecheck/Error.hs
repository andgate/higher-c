module Language.Hawk.Typecheck.Error where

import Language.Hawk.Syntax.Abstract

data TcError
  = TypeMismatch Type Type