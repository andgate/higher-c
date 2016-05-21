-- This will be implemented along with classes and instances
module Language.Hawk.AST.QType where

import qualified Language.Hawk.AST.Name as Name
import qualified Language.Hawk.AST.Type as Type
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


type Source
  = QType Name.Raw R.Region

type Valid
  = QType Name.Raw R.Region

type Canonical
  = QType Name.Canonical R.Region

type Typed
  = QType Name.Canonical R.Region


data QType n a
  = QType (Context n a) (Type.Type n a) a

data Context n a
  = Context [ClassAsst n a] a


-- Currently class assertions may only be performed on a set of
-- TVar's
data ClassAsst n a =
  ClassAsst n [Type.TVar] a