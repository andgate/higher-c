{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Syntax.TypeDeclaration where

import Data.Binary
import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.OpInfo as OI
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T

type Source = 
  TypeDecl N.Source T.Source
  
type Valid = 
  TypeDecl N.Valid T.Valid

type Typed = 
  TypeDecl N.Typed T.Typed

data TypeDecl n t
  = TypeDecl
    { tydefContext :: QT.Context t
    , tydefName :: n
    , tydefOpinfo :: OI.OpInfo
    , tydefTyvars :: [t]
    }
  deriving (Eq, Show, Ord, Data, Typeable)
  
  
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (TypeDecl n t) where
    pretty (TypeDecl c n oi vs) =
      PP.text "Type Declaration:"
      PP.<$>
      PP.indent 2
        ( 
          PP.text "Name:" <+> PP.pretty n
          PP.<$>
          PP.text "OpInfo:" <+> PP.pretty oi
          PP.<$>
          PP.text "Context:" <+> PP.pretty c
          PP.<$>
          PP.text "Vars:" <+> PP.pretty vs
        )
        
  
instance (Binary n, Binary t) => Binary (TypeDecl n t) where
  get =
    TypeDecl <$> get <*> get <*> get <*> get

  put (TypeDecl c n oi v) =
    put c >> put n >> put oi >> put v