module Language.Hawk.Syntax.Binding where

import Data.Binary
import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Name as Name

type Source =
  Binding Name.Source
  
type Valid =
  Binding Name.Valid

type Typed =
  Binding Name.Typed

data Binding n
  = Binding
    { mode  :: Mode
    , label :: n
    }
  deriving (Eq, Show, Ord, Data, Typeable)
  
data Mode
  = ByRef Mutability
  | ByVal Mutability
  deriving (Eq, Show, Ord, Data, Typeable)
  
data Mutability
  = Mutable
  | Immutable
  deriving (Eq, Show, Ord, Data, Typeable)
  
  
isRef :: Mode -> Bool
isRef (ByRef _) = True
isRef _ = False

isMut :: Mode -> Bool
isMut (ByRef Mutable) = True
isMut (ByVal Mutable) = True
isMut _ = False
  

instance (PP.Pretty n) => PP.Pretty (Binding n) where
  pretty (Binding m l) =
    PP.text "Binding:"
    PP.<$>
    PP.indent 2
      ( PP.text "mode:" <+> PP.pretty m
        PP.<$>
        PP.text "label:" <+> PP.pretty l
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
    
    
      
instance (Binary n) => Binary (Binding n) where
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