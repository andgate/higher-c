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
  = DepDecl Dependency
  | ExprDef (ED.ExprDef n e t)
  | AliasDef (AD.AliasDef n t)
  | DataDef (DD.DataDef n t)
  | TypeClassDef (TCD.TypeClassDef n e t)
  deriving (Eq, Show, Data, Typeable)


data Dependency =
  Dep
    { isPropagated :: Bool
    , isQualForced :: Bool
    , depPath :: DepPath
    , depAlias :: Maybe Text
    } deriving (Eq, Show, Data, Typeable)

data DepPath = 
    DepModule  Text DepPath
  | DepTarget  Text
  | DepTargets Bool [DepPath]
  deriving (Eq, Show, Data, Typeable)

  
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Item n e t) where
    pretty (DepDecl dd) =
      PP.pretty dd
        
    pretty (ExprDef ed) =
      PP.pretty ed
      
    pretty (AliasDef ta) =
      PP.pretty ta
    
    pretty (DataDef dd) =
      PP.pretty dd
      
    pretty (TypeClassDef cd) =
      PP.pretty cd
      

instance PP.Pretty Dependency where
    pretty (Dep pr ql p a) =
      PP.text "Import:"
      PP.<$>
      PP.indent 2
        ( PP.text "Is Propagated:" PP.<+> PP.pretty pr
          PP.<$>
          PP.text "Is Qualified:" PP.<+> PP.pretty ql
          PP.<$>
          PP.text "Path:" PP.<+> PP.pretty p
          PP.<$>
          PP.text "Alias:" PP.<+> PP.text (show (Text.unpack <$> a))
        )

      
instance PP.Pretty DepPath where
    pretty (DepModule n r) =
      PP.text (Text.unpack n) PP.<> PP.pretty r
        
    pretty (DepTarget n) =
      PP.text (Text.unpack n)
        
    pretty (DepTargets True rs) =
      PP.text "(" PP.<> PP.pretty rs PP.<> PP.text ")"

    pretty (DepTargets False rs) =
      PP.text "(\\" PP.<> PP.pretty rs PP.<> PP.text ")"