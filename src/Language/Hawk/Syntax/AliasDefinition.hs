module Language.Hawk.Syntax.AliasDefinition where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDeclaration as TD
import qualified Text.PrettyPrint.ANSI.Leijen as PP


type Source = 
  AliasDef N.Source T.Source
  
type Valid = 
  AliasDef N.Valid T.Valid

type Typed = 
  AliasDef N.Typed T.Typed


data AliasDef n t
    = AliasDef
      { alias_decl :: TD.TypeDecl n t
      , alias_body :: t
      }
    deriving (Eq, Show, Data, Typeable)
    
    
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (AliasDef n t) where
    pretty (AliasDef d b) =
      PP.text "Alias Definition:"
      PP.<$>
      PP.indent 2
        ( PP.text "decl:" <+> PP.pretty d
          PP.<$>
          PP.text "body:" <+> PP.pretty b
        )
        
        
instance (Binary n, Binary t) => Binary (AliasDef n t) where
  get =
    AliasDef <$> get <*> get

  put (AliasDef d b) =
    put d >> put b