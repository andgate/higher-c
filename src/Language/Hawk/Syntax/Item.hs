module Language.Hawk.Syntax.Item where

import Data.Data
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Data.Text.Lazy as Text
import qualified Language.Hawk.Syntax.ClassDefinition as CD
import qualified Language.Hawk.Syntax.ClassInstance as CI
import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.ExpressionDefinition as ED
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Record as R
import qualified Language.Hawk.Syntax.TaggedUnion as TU
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDefinition as TD
import qualified Text.PrettyPrint.ANSI.Leijen as PP

  
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
  | TypeDef (TD.TypeDef n t)
  | Record (R.Record n t)
  | TaggedUnion (TU.TaggedUnion n t)
  | ClassDef (CD.ClassDef n t)
  | ClassInst (CI.ClassInst n e t)
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
    
    pretty (TypeDef td) =
      PP.pretty td
       
    pretty (Record r) =
      PP.pretty r
      
    pretty (TaggedUnion tu) =
      PP.pretty tu
      
    pretty (ClassDef cd) =
      PP.pretty cd
      
    pretty (ClassInst ci) =
      PP.pretty ci