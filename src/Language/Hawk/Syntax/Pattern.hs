{-# LANGUAGE  TemplateHaskell
            , LambdaCase
            , FlexibleInstances
            , DeriveGeneric
            , DeriveDataTypeable
            , OverloadedStrings
            , StandaloneDeriving
            , MultiParamTypeClasses
  #-}
module Language.Hawk.Syntax.Pattern where

import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Data
import Data.Default.Class
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Kind
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.Literal

import qualified Text.PrettyPrint.Leijen.Text as PP


------------------------------------------------------------------------
-- Pattern

data Pat
  = PVar Text
  | PLit Lit
  | PCon Text [Pat]
  | PMask Text Pat
  | PParen Pat
  | PLoc Loc Pat
  | PType Type Pat
  deriving(Show,Read,Ord,Eq,Data,Typeable,Generic)


instance Binary Pat
instance Plated Pat
instance FromJSON Pat
instance ToJSON Pat

instance HasKind Pat where
  kind = \case
    PVar n -> undefined


instance PP.Pretty Pat where
  pretty = \case
    PVar n ->
      PP.textStrict n

    PLit l ->
      PP.pretty l

    PCon n ps ->
      PP.textStrict n PP.<+> PP.pretty ps

    PMask n p ->
      PP.pretty n
        PP.<> PP.textStrict "@"
        PP.<> PP.pretty p

    PParen p ->
      PP.parens (PP.pretty p)

    PLoc _ p ->
      PP.pretty p -- Usually best to hide locations

    PType t p ->
      PP.pretty p
        PP.<+> PP.textStrict "::"
        PP.<+> PP.pretty t


    
