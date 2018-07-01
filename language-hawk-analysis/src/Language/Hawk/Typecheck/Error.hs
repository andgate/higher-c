module Language.Hawk.Typecheck.Error where

import Data.Text (Text)
import Language.Hawk.Syntax.Suspension

data TcError
  = TypeMismatch (Type Text) (Type Text)