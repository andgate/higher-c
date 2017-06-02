{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Syntax.Item.Type.Signature where

import Data.Binary
import Data.Data
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))

import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Type as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP


type Source = 
  Signature N.Source T.Source
  
type Valid = 
  Signature N.Valid T.Valid

type Typed = 
  Signature N.Typed T.Typed


data Signature n t
    = Sig
      { tySigName :: n
      , tySigBody :: t
      }
    deriving (Eq, Show, Ord, Data, Typeable)
    
    
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (Signature n t) where
    pretty (Sig name body) =
      PP.text "Type Signature Item:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" <+> PP.pretty name
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )
        
        
instance (Binary n, Binary t) => Binary (Signature n t) where
  get =
    Sig <$> get <*> get

  put (Sig name body) =
    put name >> put body