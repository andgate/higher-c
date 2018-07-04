module Language.Hawk.Typecheck.Error where

import Language.Hawk.Syntax.Bound

data TcError
  = TypeMismatch Type Type