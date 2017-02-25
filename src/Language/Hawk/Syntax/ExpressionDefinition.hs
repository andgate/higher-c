module Language.Hawk.Syntax.ExpressionDefinition where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Data.Text.Lazy                   as Text
import qualified Text.PrettyPrint.ANSI.Leijen     as PP

import qualified Language.Hawk.Parse.Lexer        as Lex
import qualified Language.Hawk.Syntax.Expression  as E
import qualified Language.Hawk.Syntax.Name        as N
import qualified Language.Hawk.Syntax.OpInfo      as OI
import qualified Language.Hawk.Syntax.Type        as T


type Source
  = ExprDef N.Source E.Source T.Source

type Valid
  = ExprDef N.Valid E.Valid T.Valid

type Typed
  = ExprDef N.Typed E.Typed T.Typed


data ExprDef n e t
  = ExprDef 
    { expr_op    :: OI.OpInfo
    , expr_name  :: n
    , expr_type  :: t
    , expr_rhs   :: e
    }
  deriving (Eq, Show, Ord, Data, Typeable)

 
mkExprDef :: OI.OpInfo -> N.Source -> [N.Source] -> T.Source -> E.Source -> Source
mkExprDef oi n vs t e =
  ExprDef oi n t (Lex.mkLam vs e)

      
      
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (ExprDef n e t) where
  pretty (ExprDef opinf name vars tipe rhs) =
    PP.text "Expression Definition:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "op info:" <+> PP.pretty opinf
        PP.<$>
        PP.text "vars:" <+> PP.pretty vars
        PP.<$>
        PP.text "type:" <+> PP.pretty tipe
        PP.<$>
        PP.text "rhs:" <+> PP.pretty rhs
      )
  
  
instance (Binary n, Binary e, Binary t) => Binary (ExprDef n e t) where
  get =
      ExprDef <$> get <*> get <*> get <*> get <*> get
      
  put (ExprDef oi n vs t rhs) =
      put oi >> put n >> put vs >> put t >> put rhs