module Language.Hawk.Rename.Error where

import Data.Text (Text)

data RnError
  = UndeclaredName Text