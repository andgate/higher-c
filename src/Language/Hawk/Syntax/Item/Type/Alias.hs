{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Syntax.Item.Type.Alias where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Type as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP


type Source = 
  Alias N.Source T.Source
  
type Valid = 
  Alias N.Valid T.Valid

type Typed = 
  Alias N.Typed T.Typed


data Alias n t
    = Alias
      { aliasName :: n
      , aliasTyVars :: [n]
      , aliasBody :: t
      }
    deriving (Eq, Show, Data, Typeable)
    
    
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (Alias n t) where
    pretty (Alias name tyvars body) =
      PP.text "Type Alias Item:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" <+> PP.pretty name
          PP.<$>
          PP.text "tyvars:" <+> PP.pretty tyvars
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )
        
        
instance (Binary n, Binary t) => Binary (Alias n t) where
  get =
    Alias <$> get <*> get <*> get

  put (Alias name tyvars body) =
    put name >> put tyvars >> put body