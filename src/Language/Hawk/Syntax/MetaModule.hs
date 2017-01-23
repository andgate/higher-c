module Language.Hawk.Syntax.MetaModule where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Data.Map                         as Map
import qualified Language.Hawk.Syntax.MetaItem    as MT
import qualified Language.Hawk.Syntax.ModuleName  as ModuleName
import qualified Language.Hawk.Syntax.Name        as Name
import qualified Language.Hawk.Syntax.Type        as Type
import qualified Text.PrettyPrint.ANSI.Leijen     as PP


type Source =
  MetaModule Name.Source (Maybe Type.Source) 
  
type Valid =
  MetaModule Name.Valid (Maybe Type.Valid)
  
type Typed =
  MetaModule Name.Typed (Maybe Type.Typed)
    
data MetaModule n t
  = MetaModule [MT.MetaItem n t]
    deriving(Eq, Show, Data, Typeable)



instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (MetaModule n t) where
  pretty (MetaModule its) =
    PP.text "Meta-Module:"
    PP.<$>
    PP.indent 2
      ( 
        PP.text "items:" <+> PP.pretty its
      )
      
      
instance (Binary n, Binary t) => Binary (MetaModule n t) where
  get =
    MetaModule <$> get
    
  put (MetaModule its) =
    put its