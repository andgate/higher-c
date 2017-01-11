{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Language.Hawk.Report.Annotation where

import Prelude hiding (map)
import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Data.Binary
import Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Data.Data
import Data.Typeable

import qualified Language.Hawk.Report.Region as R


data Annotated annot a
  = A annot a
  deriving (Eq, Show, Data, Typeable)
  
type Located a =
  Annotated R.Region a
 
-- This is currently unused 
type Commented a =
  Annotated (R.Region, Maybe String) a
  

  
at :: R.Position -> R.Position -> a -> Located a
at start end value =
  A (R.Region start end) value
  
  
merge :: Located a -> Located b -> value -> Located value
merge (A region1 _) (A region2 _) value =
  A (R.merge region1 region2) value
  
  
sameAs :: Annotated info a -> b -> Annotated info b
sameAs (A annot _)  value =
  A annot value
  
  
map :: (a -> b) -> Annotated info a -> Annotated info b
map f (A annot value) =
  A annot (f value)
  
drop :: Annotated info a -> a
drop (A _ value) =
  value


instance (PP.Pretty annot, PP.Pretty a) => PP.Pretty (Annotated annot a) where
  pretty (A annot a) =
    PP.pretty a <+> PP.parens (PP.pretty annot)


instance (Json.ToJSON annot, Json.ToJSON a) => Json.ToJSON (Annotated annot a) where
  toJSON (A annot a) =
    Json.object
      [ "annot" .= annot
      , "value" .= a
      ]
  
  
instance (Binary annot, Binary a) => Binary (Annotated annot a) where
  put (A annot a) =
    put annot >> put a
    
    
  get =
    A <$> get <*> get