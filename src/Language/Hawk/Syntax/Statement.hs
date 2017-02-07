module Language.Hawk.Syntax.Statement where

import Data.Binary
import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Variable as Var


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


data Statement n e t
  = Do (Block n e t)
  | Call e
  | Let (Var.Variable n e t)
  | Assign n t e
  | Break
  | Return e
  | If [(e, Block n e t)] (Block n e t)
  | While e (Block n e t)
  deriving (Eq, Show, Ord, Data, Typeable)
  
  

instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Statement n e t) where
  pretty (Do e) =
    PP.text "Do Statement:"
    PP.<$>
    PP.indent 2 ( PP.pretty e )
    
  pretty (Call e) =
    PP.text "Call Statement:"
    PP.<$>
    PP.indent 2 
        ( PP.pretty e )
        
  pretty (Let var) =
    PP.text "Let Statement:"
    PP.<$>
    PP.indent 2
      ( PP.string "Variable:" <+> PP.pretty var
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
  
  
instance (Binary n, Binary e, Binary t) => Binary (Statement n e t) where
    get = do
        n <- getWord8
        case n of
          1 -> Do <$> get
          2 -> Call <$> get
          3 -> Let <$> get
          4 -> Assign <$> get <*> get <*> get
          5 -> return Break
          6 -> Return <$> get
          7 -> If <$> get <*> get
          8 -> While <$> get <*> get
      
    put e =
        case e of
          Do blk        -> putWord8 1 >> put blk
          Call e        -> putWord8 2 >> put e
          Let v         -> putWord8 3 >> put v
          Assign n t e  -> putWord8 4 >> put n >> put t >> put e
          Break         -> putWord8 5
          Return e      -> putWord8 6 >> put e
          If ps e       -> putWord8 7 >> put ps >> put e
          While e blk   -> putWord8 8 >> put e >> put blk