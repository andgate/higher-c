module Language.Hawk.AST.Function where

import qualified Language.Hawk.AST.QType as QType
import qualified Language.Hawk.AST.Statement as Stmt


data Function n e a t
  = Function 
    { fn_name :: n
    , fn_type :: t
    , fn_args :: [n]
    , fn_body :: Stmt.Block n e a t
    , fn_annot :: a
    }