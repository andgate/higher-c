{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Syntax.Item.Type.Class where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))


import qualified Language.Hawk.Syntax.Item.Type.Signature as S
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.QType as QT
import qualified Language.Hawk.Syntax.Type as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP


type Source = 
  Class N.Source T.Source
  
type Valid = 
  Class N.Valid T.Valid

type Typed = 
  Class N.Typed T.Typed

data Class n t
    = Class 
      { tyClassContext :: QT.Context t
      , tyClassName :: n
      , tyClassVars :: [n]
      , tyClassBody :: [S.Signature n t]
      }
    deriving (Eq, Show, Ord, Data, Typeable)


    
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (Class n t) where
    pretty (Class ctx name tyvars body) =
      PP.text "Type Class Item"
      PP.<$>
      PP.indent 2
        ( PP.text "context:" <+> PP.pretty ctx
          PP.<$>
          PP.text "name:" <+> PP.pretty name
          PP.<$>
          PP.text "type vars:" <+> PP.pretty tyvars
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )
        
        
instance (Binary n, Binary t) => Binary (Class n t) where
  get =
    Class <$> get <*> get <*> get <*> get

  put (Class ctx name tyvars body) =
    put ctx >> put name >> put tyvars >> put body