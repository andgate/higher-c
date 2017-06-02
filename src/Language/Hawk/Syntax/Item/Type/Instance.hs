{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Syntax.Item.Type.Instance where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.ExpressionDefinition as ED
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Syntax.TypeDeclaration as TD
import qualified Text.PrettyPrint.ANSI.Leijen as PP


type Source = 
  TypeClassDef N.Source E.Source T.Source
  
type Valid = 
  TypeClassDef N.Valid E.Valid T.Valid

type Typed = 
  TypeClassDef N.Typed E.Typed T.Typed

data TypeClassDef n e t
    = TypeClassDef 
      { tcDecl :: TD.TypeDecl n t
      , tcBody :: [ED.ExprDef n e t]
      }
    deriving (Eq, Show, Data, Typeable)
    
    
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (TypeClassDef n e t) where
    pretty (TypeClassDef d b) =
      PP.text "TypeClass Definition:"
      PP.<$>
      PP.indent 2
        ( PP.text "decl:" <+> PP.pretty d
          PP.<$>
          PP.text "body:" <+> PP.pretty b
        )
        
        
instance (Binary n, Binary e, Binary t) => Binary (TypeClassDef n e t) where
  get =
    TypeClassDef <$> get <*> get

  put (TypeClassDef d b) =
    put d >> put b