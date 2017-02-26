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
  TypeDef N.Source T.Source T.Source
  
type Valid =
  TypeDef N.Valid T.Valid T.Valid
  
type Typed =
  TypeDef N.Typed T.Typed T.Typed


data TypeDef n t b
  = TypeDef
    { tydef_context :: QT.Context t
    , tydef_name :: n
    , tydef_tyvars :: [n]
    , tydef_body :: b
    }
  deriving (Eq, Show, Ord, Data, Typeable)


mkTypeDef :: n -> [n] -> t -> TypeDef n t b
mkTypeDef n vs t =
  TypeDef QT.emptyCtx n vs t
  
  
instance (PP.Pretty n, Binary t, PP.Pretty b) => PP.Pretty (TypeDef n t b) where
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
        
  
instance (Binary n, Binary t, Binary b) => Binary (TypeDef n t b) where
  get =
    TypeDef <$> get <*> get <*> get <*> get

  put (TypeDef c n v b) =
    put c >> put n >> put v >> put b