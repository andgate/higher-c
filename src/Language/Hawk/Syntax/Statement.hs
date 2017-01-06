module Language.Hawk.Syntax.Statement where

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Object as Obj
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R

type Source
  = Statement Name.Source Expr.Source (Maybe Type.Source)
  
type SourceBlock
  = Block Name.Source Expr.Source (Maybe Type.Source)


type Valid
  = Statement Name.Valid Expr.Valid (Maybe Type.Valid)
  
type ValidBlock
  = Block Name.Valid Expr.Valid (Maybe Type.Valid)


type Canonical
  = Statement Name.Canonical Expr.Canonical (Maybe Type.Canonical)

type CanonicalBlock
  = Block Name.Canonical Expr.Canonical (Maybe Type.Canonical)


type Typed
  = Statement Name.Typed Expr.Typed Type.Typed
  
type TypedBlock
  = Block Name.Typed Expr.Typed Type.Typed
  


type Block n e t = [Statement n e t]


type Statement n e t =
  A.Located (Statement' n e t)


data Statement' n e t
  = Do (Block n e t)
  | Call e
  | Let (Obj.Object n e t)
  | Assign n t e
  | Break
  | Return e
  | If [(e, Block n e t)] (Block n e t)
  | While e (Block n e t)
  deriving (Show)
  
  
mkRetBlk :: Expr.Source -> SourceBlock
mkRetBlk e@(A.A r _) =
  [A.A r (Return e)]
  
  

instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Statement' n e t) where
  pretty (Do e) =
    PP.text "Do Statement:"
    PP.<$>
    PP.indent 2 ( PP.pretty e )
    
  pretty (Call e) =
    PP.text "Call Statement:"
    PP.<$>
    PP.indent 2 
        ( PP.pretty e )
        
  pretty (Let object) =
    PP.text "Let Statement:"
    PP.<$>
    PP.indent 2
      ( PP.string "Obj:" <+> PP.pretty object
      )
      
  pretty (Assign name tipe e) =
    PP.text "Assign Statement:"
    PP.<$>
    PP.indent 2
      ( PP.string "name:" <+> PP.pretty name
        <+> PP.string "type:" <+> PP.pretty tipe
        <+> PP.string "expr:" <+> PP.pretty e
      )
      
  pretty (Return e) =
    PP.text "Return Statement:"
    PP.<$>
    PP.indent 2 
        ( PP.pretty e )        
  