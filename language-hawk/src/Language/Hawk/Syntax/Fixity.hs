{-# Language DeriveGeneric, LambdaCase, OverloadedStrings #-}
module Language.Hawk.Syntax.Fixity where

import Data.Aeson
import Data.Binary
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Location

import Data.Text.Prettyprint.Doc


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

instance Pretty Fixity where
  pretty (Fixity f p ops) =
    vcat
      [ "Fixity Declaration"
      , indent 2 $ vcat
        [ "Fixity:" <+> pretty (pack $ show f)
        , "Precedence:" <+> pretty (pack $ show p)
        , "Operators:" <+> pretty ops
        ]
      ]
