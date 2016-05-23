-- This will be implemented along with classes and instances
module Language.Hawk.Syntax.QType where

import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Source
  = QType Name.Raw

type Valid
  = QType Name.Raw

type Canonical
  = QType Name.Canonical

type Typed
  = QType Name.Canonical


data QType n
  = QType (Context n) (Type.Type n)

data Context n
  = Context [ClassAsst n]


-- Currently class assertions may only be performed on a set of
-- TVar's
data ClassAsst n =
  ClassAsst n [Type.TVar]