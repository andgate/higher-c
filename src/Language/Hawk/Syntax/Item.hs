module Language.Hawk.Syntax.Item where

import Data.Data
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Data.Text.Lazy as Text
import qualified Language.Hawk.Syntax.Alias as A
import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.Function as F
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Record as R
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.Variable as V
import qualified Text.PrettyPrint.ANSI.Leijen as PP

  
-- Items Structure
  
type Source = 
  Item N.Source E.Source (Maybe T.Source)
 
type Valid = 
  Item N.Valid E.Valid (Maybe T.Valid)
  
type Typed =
  Item N.Typed E.Typed T.Typed
   
data Item n e t
  = Import N.Paths
  | Export N.Paths
  | Function (F.Function n e t)
  | Variable (V.Variable n e t)
  | Record (R.Record n)
  | Alias (A.Alias n)
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
        
    pretty (Function fn) =
      PP.pretty fn
        
    pretty (Variable var) =
      PP.pretty var
        
    pretty (Record r) =
      PP.pretty r
      
    pretty (Alias a) =
      PP.pretty a