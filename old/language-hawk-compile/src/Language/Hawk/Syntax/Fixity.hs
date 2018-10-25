{-# Language DeriveGeneric, LambdaCase, OverloadedStrings #-}
module Language.Hawk.Syntax.Fixity where

import Data.Aeson
import Data.Binary
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Location

import qualified Text.PrettyPrint.Leijen.Text as PP


-- -----------------------------------------------------------------------------
-- | Fixity

-- A fixity declaration
data Fixity = Fixity FixityKind (L Int) [L Text]
  deriving (Show, Eq, Generic)

instance Binary Fixity
instance FromJSON Fixity
instance ToJSON Fixity


-- The kinds of fixity
data FixityKind
  = InfixN
  | InfixL
  | InfixR
  | Prefix
  | Postfix
  deriving (Show, Eq, Generic)

instance Binary FixityKind
instance FromJSON FixityKind
instance ToJSON FixityKind


-------------------------------------------------------------------------
-- Pretty Printing

instance PP.Pretty Fixity where
  pretty (Fixity f p ops) =
    PP.textStrict "Fixity Declaration"
    PP.<$>
    PP.indent 2
    ( PP.textStrict "Fixity:" PP.<+> PP.textStrict (pack $ show f)
      PP.<$>
      PP.textStrict "Precedence:" PP.<+> PP.textStrict (pack $ show p)
      PP.<$>
      PP.textStrict "Operators:" PP.<+> PP.pretty ops
    )
