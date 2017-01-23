module Language.Hawk.Syntax.Item where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Data.Text.Lazy as Text
import qualified Language.Hawk.Syntax.Alias as Alias
import qualified Language.Hawk.Syntax.Expression as Expr
import qualified Language.Hawk.Syntax.Function as Fn
import qualified Language.Hawk.Syntax.ModuleName as ModuleName
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Record as Record
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Text.PrettyPrint.ANSI.Leijen as PP

  
-- Items Structure
  
type Source = 
  Item Name.Source Expr.Source (Maybe Type.Source)
 
type Valid = 
  Item Name.Valid Expr.Valid (Maybe Type.Valid)
  
type Canonical =
  Item Name.Canonical Expr.Canonical (Maybe Type.Canonical)
  
type Typed =
  Item Name.Typed Expr.Typed Type.Typed
   
data Item n e t
  = ImportItem Visibility (ModuleName.Raw)
  | FunctionItem Visibility (Fn.Function n e t)
  | VarItem Visibility (Var.Variable n e t)
  | RecordItem Visibility (Record.Record n)
  | AliasItem Visibility (Alias.Alias n)
  deriving (Eq, Show, Data, Typeable)
  
  
data Visibility
  = Public
  | Private
  deriving (Eq, Show, Data, Typeable)

impItem :: ModuleName.Raw -> Item n e t
impItem = ImportItem Public

fnItem :: Fn.Function n e t -> Item n e t
fnItem = FunctionItem Public

varItem :: Var.Variable n e t -> Item n e t
varItem = VarItem Public

recItem :: Record.Record n -> Item n e t
recItem = RecordItem Public

aliasItem :: Alias.Alias n -> Item n e t
aliasItem = AliasItem Public


findImports :: [Source] -> [Source]
findImports = filter isImport

isImport :: Item n e t -> Bool
isImport (ImportItem _ _) = True
isImport _ = False
  
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Item n e t) where
    pretty (ImportItem v n) =
      PP.text "ImportItem:"
      PP.<$>
      PP.indent 2
        ( PP.text "visibility:" <+> PP.pretty v
          PP.<$>
          PP.text "name:" <+> PP.pretty (ModuleName.toStringRaw n)
        )
        
    pretty (FunctionItem v fn) =
      PP.text "FunctionItem:"
      PP.<$>
      PP.indent 2
        ( PP.text "visibility:" <+> PP.pretty v
          PP.<$>
          PP.text "function:" <+> PP.pretty fn
        )
        
    pretty (VarItem v var) =
      PP.text "VarItem:"
      PP.<$>
      PP.indent 2
        ( PP.text "visibility:" <+> PP.pretty v
          PP.<$>
          PP.text "var:" <+> PP.pretty var
        )
        
    pretty (RecordItem v r) =
      PP.text "RecordItem:"
      PP.<$>
      PP.indent 2
        ( PP.text "visibility:" <+> PP.pretty v
          PP.<$>
          PP.text "record:" <+> PP.pretty r
        )
      
    pretty (AliasItem v a) =
      PP.text "AliasItem:"
      PP.<$>
      PP.indent 2
        ( PP.text "visibility:" <+> PP.pretty v
          PP.<$>
          PP.text "alias:" <+> PP.pretty a
        )
        
        
instance PP.Pretty Visibility where
  pretty Public = PP.text "Public"
  pretty Private = PP.text "Private"