module Language.Hawk.Syntax.TypeDefinition where

import Data.Binary
import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T


type Source =
  TypeDef N.Source T.Source
  
type Valid =
  TypeDef N.Valid T.Valid
  
type Typed =
  TypeDef N.Typed T.Typed


data TypeDef n b
  = TypeDef
    { tydef_context :: QT.Context n
    , tydef_name :: n
    , tydef_tyvars :: [n]
    , tydef_body :: b
    }
  deriving (Eq, Show, Ord, Data, Typeable)

  
instance (PP.Pretty n, PP.Pretty b) => PP.Pretty (TypeDef n b) where
    pretty (TypeDef c n vs b) =
      PP.text "Type definition:"
      PP.<$>
      PP.indent 2
        ( PP.text "Name:" <+> PP.pretty n
          PP.<$>
          PP.text "Context:" <+> PP.pretty c
          PP.<$>
          PP.text "Vars:" <+> PP.pretty vs
          PP.<$>
          PP.text "Body:" <+> PP.pretty b
        )
        
  
instance (Binary n, Binary b) => Binary (TypeDef n b) where
  get =
    TypeDef <$> get <*> get <*> get <*> get

  put (TypeDef c n v b) =
    put c >> put n >> put v >> put b