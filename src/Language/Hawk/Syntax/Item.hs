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
import qualified Language.Hawk.Syntax.ModuleName as MN
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
  
type Canonical =
  Item N.Canonical E.Canonical (Maybe T.Canonical)
  
type Typed =
  Item N.Typed E.Typed T.Typed
   
data Item n e t
  = Import Visibility (MN.Raw)
  | Function Visibility (F.Function n e t)
  | Variable Visibility (V.Variable n e t)
  | Record Visibility (R.Record n)
  | Alias Visibility (A.Alias n)
  deriving (Eq, Show, Ord, Data, Typeable)
  
  
data Visibility
  = Public
  | Private
  deriving (Eq, Show, Ord, Data, Typeable)

impItem :: MN.Raw -> Item n e t
impItem = Import Public

fnItem :: F.Function n e t -> Item n e t
fnItem = Function Public

varItem :: V.Variable n e t -> Item n e t
varItem = Variable Public

recItem :: R.Record n -> Item n e t
recItem = Record Public

aliasItem :: A.Alias n -> Item n e t
aliasItem = Alias Public


getDeps :: [Item n e t] -> [Text]   
getDeps = mapMaybe getDep
    
getDep :: Item n e t -> Maybe Text
getDep (Import _ n) = q n
  where q = Just . Text.pack . MN.toStringRaw
getDep _ = Nothing

  
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Item n e t) where
    pretty (Import v n) =
      PP.text "Import:"
      PP.<$>
      PP.indent 2
        ( PP.text "visibility:" <+> PP.pretty v
          PP.<$>
          PP.text "name:" <+> PP.pretty (MN.toStringRaw n)
        )
        
    pretty (Function v fn) =
      PP.text "Function:"
      PP.<$>
      PP.indent 2
        ( PP.text "visibility:" <+> PP.pretty v
          PP.<$>
          PP.text "function:" <+> PP.pretty fn
        )
        
    pretty (Variable v var) =
      PP.text "Variable:"
      PP.<$>
      PP.indent 2
        ( PP.text "visibility:" <+> PP.pretty v
          PP.<$>
          PP.text "var:" <+> PP.pretty var
        )
        
    pretty (Record v r) =
      PP.text "Record:"
      PP.<$>
      PP.indent 2
        ( PP.text "visibility:" <+> PP.pretty v
          PP.<$>
          PP.text "record:" <+> PP.pretty r
        )
      
    pretty (Alias v a) =
      PP.text "Alias:"
      PP.<$>
      PP.indent 2
        ( PP.text "visibility:" <+> PP.pretty v
          PP.<$>
          PP.text "alias:" <+> PP.pretty a
        )
        
        
instance PP.Pretty Visibility where
  pretty Public = PP.text "Public"
  pretty Private = PP.text "Private"