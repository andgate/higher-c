module Language.Hawk.Syntax.Binding where

import Data.Binary
import Data.Data
import Data.Typeable

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
  deriving (Eq, Show, Data, Typeable)
  
data Mode
  = ByRef Mutability
  | ByVal Mutability
  deriving (Eq, Show, Data, Typeable)
  
data Mutability
  = Mutable
  | Immutable
  deriving (Eq, Show, Data, Typeable)
  
  
getLabel :: Binding n -> n
getLabel (A.A _ b) = label b
  
  

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
    PP.text "&" <> PP.pretty mut

  pretty (ByVal mut) =
    PP.pretty mut
    
    
instance PP.Pretty Mutability where
  pretty Mutable =
    PP.text ""

  pretty Immutable =
    PP.text "!"
    
    
      
instance (Binary n) => Binary (Binding' n) where
  get =
    Binding <$> get <*> get

  put (Binding m l) =
    put m >> put l
    
    
instance Binary Mode where
  get = do  t <- getWord8
            case t of
                0 -> ByRef <$> get
                1 -> ByVal <$> get

  put (ByRef m) = putWord8 0 >> put m
  put (ByVal m) = putWord8 1 >> put m
  
  
instance Binary Mutability where
  get = do  t <- getWord8
            case t of
                0 -> return Mutable
                1 -> return Immutable

  put Mutable = putWord8 0
  put Immutable = putWord8 1