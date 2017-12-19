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
import Data.Monoid
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
  | PWild
  | PAs Text Pat
  | PCon Text [Pat]
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


instance Default Pat where
  def = PWild


instance PP.Pretty Pat where
  pretty = \case
    PVar n ->
      PP.textStrict n

    PLit l ->
      PP.pretty l

    PWild->
      PP.textStrict "_"

    PAs n p ->
      PP.textStrict n
        PP.<> PP.textStrict "@"
        PP.<> PP.pretty p

    PCon n ps ->
      PP.textStrict n PP.<+> PP.pretty ps

    PParen p ->
      PP.parens (PP.pretty p)

    PLoc _ p ->
      PP.pretty p -- Usually best to hide locations

    PType t p ->
      PP.pretty p
        PP.<+> PP.textStrict ":"
        PP.<+> PP.pretty t


------------------------------------------------------------------------
-- Helpers


locPat :: Pat -> Loc
locPat = \case
  PVar n -> error "No location found."
  PLit l -> error "No location found."
  PWild-> error "No location found."
  PAs n p -> locPat p
  PCon n ps -> mconcat (locPat <$> ps)
  PParen p -> locPat p
  PLoc l _ -> l
  PType t p -> locPat p <> locType t


patNames :: Pat -> [Text]
patNames = \case
  PVar n -> [n]
  PLit l -> []
  PWild-> []
  PAs n p -> n : patNames p
  PCon n ps -> n : concatMap patNames ps
  PParen p -> patNames p
  PLoc _ p -> patNames p 
  PType t p -> patNames p
