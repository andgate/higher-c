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
  
type Valid =
  Variable Name.Valid Expr.Valid (Maybe Type.Valid)
  
type Typed =
  Variable Name.Typed Expr.Typed Type.Typed


data Variable n e t
  = Variable
    { name  :: Binding.Binding n 
    , tipe  :: t
    , rhs    :: e
    }
  deriving (Eq, Show, Ord, Data, Typeable)
  
  
  
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Variable n e t) where
  pretty (Variable n t rhs) =
    PP.text "Variable:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty n
        PP.<$>
        PP.text "type:" <+> PP.pretty t
        PP.<$>
        PP.text "rhs:" <+> PP.pretty rhs
      )



instance (Binary n, Binary e, Binary t) => Binary (Variable n e t) where
  get =
    Variable <$> get <*> get <*> get

  put (Variable n t e) =
    put n >> put t >> put e