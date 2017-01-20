module Language.Hawk.Syntax.Variable where

import Data.Binary
import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Binding as Binding
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type


type Source =
  Variable Name.Source Expr.Source (Maybe Type.Source)
  
type SourceInfo =
  VariableInfo Name.Source (Maybe Type.Source)
  
type Valid =
  Variable Name.Valid Expr.Valid (Maybe Type.Valid)
  
type Canonical =
  Variable Name.Canonical Expr.Canonical (Maybe Type.Canonical)
  
type Typed =
  Variable Name.Typed Expr.Typed Type.Typed


data Variable n e t
  = Variable
    { info   :: VariableInfo n t
    , rhs    :: e
    }
  deriving (Eq, Show, Data, Typeable)
  
data VariableInfo n t
  = VariableInfo
    { name  :: Binding.Binding n 
    , tipe  :: t
    }
  deriving (Eq, Show, Data, Typeable)
  
  
  
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Variable n e t) where
  pretty (Variable info rhs) =
    PP.text "Variable:"
    PP.<$>
    PP.indent 2
      ( PP.text "info:" <+> PP.pretty info
        PP.<$>
        PP.text "rhs:" <+> PP.pretty rhs
      )

instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (VariableInfo n t) where
  pretty (VariableInfo n t) =
    PP.text "VariableInfo:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty n
        PP.<$>
        PP.text "type:" <+> PP.pretty t
      )



instance (Binary n, Binary t) => Binary (VariableInfo n t) where
  get =
    VariableInfo <$> get <*> get

  put (VariableInfo n t) =
    put n >> put t