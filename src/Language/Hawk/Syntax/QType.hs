-- This will be implemented along with classes and instances
module Language.Hawk.Syntax.QType where

import Data.Binary
import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Report.Region as R


type Source
  = QType N.Source

type Valid
  = QType N.Valid

type Typed
  = QType N.Typed


data QType n t
  = QType (Context n) t
  deriving (Eq, Show, Ord, Data, Typeable)

data Context n
  = Context [ClassAsst n]
  deriving (Eq, Show, Ord, Data, Typeable)
  
  
emptyCtx :: Context n
emptyCtx = Context []


-- Currently class assertions may only be performed on a set of
-- TVar's
data ClassAsst n
  = ClassAsst n [T.TVar]
  deriving (Eq, Show, Ord, Data, Typeable)
  
  
instance (PP.Pretty n) => PP.Pretty (Context n) where
    pretty (Context assrts) =
      PP.text "Context:"
      PP.<$>
      PP.indent 2
        ( PP.text "Assertions:" <+> PP.pretty assrts
        )
        
      
      
instance (PP.Pretty n) => PP.Pretty (ClassAsst n) where
    pretty (ClassAsst n tvs) =
      PP.text "Class Assertion:"
      PP.<$>
      PP.indent 2
        ( PP.text "ClassCons:" <+> PP.pretty n
          PP.<$>
          PP.text "Type Vars:" <+> PP.pretty tvs
        )
        
        
        
  
instance (Binary n) => Binary (Context n) where
  get =
    Context <$> get

  put (Context assrts) =
    put assrts
    
    
instance (Binary n) => Binary (ClassAsst n) where
  get =
    ClassAsst <$> get <*> get

  put (ClassAsst n tvs) =
    put n >> put tvs