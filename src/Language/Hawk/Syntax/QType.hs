{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Syntax.QType where

import Data.Binary
import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Language.Hawk.Syntax.Type as T
import qualified Language.Hawk.Report.Region as R


type Source
  = QType T.Source

type Valid
  = QType T.Source

type Typed
  = QType T.Source


data QType t
  = QType (Context t) t
  deriving (Eq, Show, Ord, Data, Typeable)

-- Types in context are validated when parsed
-- Comma seperated list of TypeClass instances,
-- which define constraints for type variables
data Context t
  = Context [t]
  deriving (Eq, Show, Ord, Data, Typeable)
  
  
emptyCtx :: Context n
emptyCtx = Context []
  
  
instance (PP.Pretty t) => PP.Pretty (Context t) where
    pretty (Context assrts) =
      PP.text "Context:"
      PP.<$>
      PP.indent 2
        ( PP.text "Assertions:" <+> PP.pretty assrts
        )
        
  
instance (Binary t) => Binary (Context t) where
  get =
    Context <$> get

  put (Context assrts) =
    put assrts