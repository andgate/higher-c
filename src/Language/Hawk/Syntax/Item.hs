module Language.Hawk.Syntax.Item where

import Data.Data
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Data.Text.Lazy                             as Text
import qualified Language.Hawk.Syntax.AliasDefinition       as AD
import qualified Language.Hawk.Syntax.DataDefinition        as DD
import qualified Language.Hawk.Syntax.Expression            as E
import qualified Language.Hawk.Syntax.ExpressionDefinition  as ED
import qualified Language.Hawk.Syntax.Name                  as N
import qualified Language.Hawk.Syntax.Type                  as T
import qualified Language.Hawk.Syntax.TypeClassDefinition   as TCD
import qualified Language.Hawk.Syntax.TypeDeclaration       as TD
import qualified Text.PrettyPrint.ANSI.Leijen               as PP

  
-- Items Structure
  
type Source = 
  Item N.Source E.Source T.Source
 
type Valid = 
  Item N.Valid E.Valid T.Valid
  
type Typed =
  Item N.Typed E.Typed T.Typed
   
data Item n e t
  = Import N.Paths
  | Export N.Paths
  | ExprDef (ED.ExprDef n e t)
  | AliasDef (AD.AliasDef n t)
  | DataDef (DD.DataDef n t)
  | TypeClassDef (TCD.TypeClassDef n e t)
  deriving (Eq, Show, Data, Typeable)


getDeps :: [Item n e t] -> [Text]   
getDeps = mapMaybe getDep
    
getDep :: Item n e t -> Maybe Text
getDep (Import ps) = q ps
  where q = Just . Text.pack . N.toString
getDep _ = Nothing

  
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Item n e t) where
    pretty (Import ps) =
      PP.text "Import:"
      PP.<$>
      PP.indent 2
        ( PP.pretty (N.toString ps) )
        
    pretty (Export ps) =
      PP.text "Export:"
      PP.<$>
      PP.indent 2
        ( PP.pretty (N.toString ps) )
        
    pretty (ExprDef ed) =
      PP.pretty ed
      
    pretty (AliasDef ta) =
      PP.pretty ta
    
    pretty (DataDef dd) =
      PP.pretty dd
      
    pretty (TypeClassDef cd) =
      PP.pretty cd