module Language.Hawk.Syntax.Alias where

import Data.Binary
import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Syntax.Type as Type

type Source =
  Alias Name.Source
  
type Valid =
  Alias Name.Valid
  
type Typed =
  Alias Name.Typed


data Alias n
  = Alias n (Type.Type n)
  deriving (Eq, Show, Ord, Data, Typeable)
  

instance (PP.Pretty n) => PP.Pretty (Alias n) where
  pretty (Alias n t) =
    PP.text "Alias:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty n
        PP.<$>
        PP.text "type:" <+> PP.pretty t
      )

instance (Binary n) => Binary (Alias n) where
  get =
    Alias <$> get <*> get

  put (Alias n t) =
    put n >> put t