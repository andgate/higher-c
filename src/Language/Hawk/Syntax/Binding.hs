module Language.Hawk.Syntax.Binding where

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Report.Annotation as A


type Source =
  Binding Name.Source
  
type Valid =
  Binding Name.Valid
  
type Canonical =
  Binding Name.Canonical

type Typed =
  Binding Name.Typed
  

type Binding n =
  A.Located (Binding' n)

data Binding' n
  = Binding
    { mode  :: Mode
    , label :: n
    }
  deriving (Show)
  
data Mode
  = ByRef Mutability
  | ByVal Mutability
  deriving (Show)
  
data Mutability
  = Mutable
  | Immutable
  deriving (Show)
  
  
instance (PP.Pretty n) => PP.Pretty (Binding' n) where
  pretty (Binding mode label) =
    PP.text "Binding:"
    PP.<$>
    PP.indent 2
      ( PP.text "mode:" <+> PP.pretty mode
        PP.<$>
        PP.text "label:" <+> PP.pretty label
      )
  
  
  
instance PP.Pretty Mode where
  pretty (ByRef mut) =
    PP.text "*" <> PP.pretty mut

  pretty (ByVal mut) =
    PP.pretty mut
    
    
instance PP.Pretty Mutability where
  pretty Mutable =
    PP.text ""
    

  pretty Immutable =
    PP.text "!"