{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Syntax.Expression where

import Data.Binary
import Data.Data
import Data.Typeable
import Language.Hawk.Parse.Lexer.Token (Token)
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Language.Hawk.Syntax.Literal as Lit
import qualified Language.Hawk.Syntax.Type    as T
import qualified Language.Hawk.Syntax.Name    as N
import qualified Text.PrettyPrint.ANSI.Leijen as PP


type Source
  = [Token]

type Valid
  = Expr N.Valid T.Valid

type Typed
  = Expr N.Typed T.Typed


data Expr n t
  = Lit Lit.Literal
  | Var n
  | Con n
  
  | Lam [n] (Expr n t)
  
  | App (Expr n t) [Expr n t]
  | Let n (Expr n t) (Expr n t)
  -- Probably need to add suport for operators later
  
  | If [Expr n t] (Expr n t)
  
  -- Specific stucture access
  | Access (Expr n t) (Expr n t)
  | RefAccess (Expr n t) (Expr n t)
  
  | Cast (Expr n t) t
  
  | Bottom
  
  | Src Source
  
  deriving (Eq, Show, Ord, Data, Typeable)


instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (Expr n t ) where
  pretty (Lit lit) =
    PP.text "Literal Expression:"
    PP.<$>
    PP.indent 2 ( PP.pretty lit )
    
  pretty (Var name) =
    PP.text "Variable Expression:"
    PP.<$>
    PP.indent 2 ( PP.pretty name )
    
  pretty (Con name) =
    PP.text "Constructor Expression:"
    PP.<$>
    PP.indent 2 ( PP.pretty name )
    
  pretty (App f xs) =
    PP.text "Application Expression:"
    PP.<$>
    PP.indent 2 
        ( PP.pretty f PP.<$> PP.pretty xs )
        
  pretty (Let name value definition) =
    PP.text "Let Expression:"
    PP.<$>
    PP.indent 2
      ( PP.string "name:" <+> PP.pretty name
        PP.<$>
        PP.string "value:" <+> PP.pretty value
        PP.<$> 
        PP.string "definition:" <+> PP.pretty definition
      )
      
      
  pretty (Cast e t) =
    PP.text "Cast Expression:"
    PP.<$>
    PP.indent 2
      ( PP.string "expression:" <+> PP.pretty e
        PP.<$>
        PP.string "type:" <+> PP.pretty t
      )
      

instance (Binary n, Binary t) => Binary (Expr n t) where
  get = do
    n <- getWord8
    case n of
      1 -> Lit <$> get
      2 -> Var <$> get
      3 -> Con <$> get
      4 -> App <$> get <*> get
      5 -> Let <$> get <*> get <*> get
      6 -> If <$> get <*> get
      7 -> Access <$> get <*> get
      8 -> RefAccess <$> get <*> get
      9 -> Cast <$> get <*> get
      
  put e =
    case e of
      Lit v           -> putWord8 1 >> put v
      Var n           -> putWord8 2 >> put n
      Con n           -> putWord8 3 >> put n
      App f a         -> putWord8 4 >> put f >> put a
      Let n e1 e2     -> putWord8 5 >> put n >> put e1 >> put e2
      If ps d         -> putWord8 6 >> put ps >> put d
      Access e1 e2    -> putWord8 7 >> put e1 >> put e2
      RefAccess e1 e2 -> putWord8 8 >> put e1 >> put e2
      Cast e1 t       -> putWord8 9 >> put e1 >> put t
              
  
          