{-# Language DeriveGeneric, LambdaCase, OverloadedStrings #-}
module Language.Hawk.Syntax.Decl where

import Data.Text
import Language.Hawk.Syntax.Term
import GHC.Generics (Generic)

import qualified Text.PrettyPrint.Leijen.Text as PP


data Decl
  = Sig TName Term
  | Def TName Term
  | RecDef TName Term
  | Foreign Foreign
  | Fixity Fixity Int [Text]
  | EmptyDecl
  deriving (Show, Generic)


-- -----------------------------------------------------------------------------
-- | Foreign

data Foreign
  = ForeignImport ForeignType Text Text Type
  | ForeignExport ForeignType Text
  deriving (Show, Generic)


data ForeignType =
  ForeignC
  deriving (Show, Eq, Generic)

-- -----------------------------------------------------------------------------
-- | Fixity

data Fixity
  = InfixN
  | InfixL
  | InfixR
  | Prefix
  | Postfix
  deriving (Show, Eq, Generic)


-- -----------------------------------------------------------------------------
-- | Pretty Printing
instance PP.Pretty Decl where
  pretty = \case
    Sig n t ->
      PP.textStrict "Signature Declaration"
      PP.<$>
      PP.indent 2
      ( PP.textStrict "Name:" PP.<+> PP.pretty n
        PP.<$>
        PP.textStrict "Sig:" PP.<+> PP.pretty t
      )

    Def n t ->
      PP.textStrict "Definition"
      PP.<$>
      PP.indent 2
      ( PP.textStrict "Name:" PP.<+> PP.pretty n
        PP.<$>
        PP.textStrict "Def:" PP.<+> PP.pretty t
      )

    RecDef n t ->
      PP.textStrict "Recursive Definition"
      PP.<$>
      PP.indent 2
      ( PP.textStrict "Name:" PP.<+> PP.pretty n
        PP.<$>
        PP.textStrict "Def:" PP.<+> PP.pretty t
      )

    Foreign f ->
      PP.pretty f

    Fixity f p ops ->
      PP.textStrict "Fixity Declaration"
      PP.<$>
      PP.indent 2
      ( PP.textStrict "Fixity:" PP.<+> PP.textStrict (pack $ show f)
        PP.<$>
        PP.textStrict "Precedence:" PP.<+> PP.textStrict (pack $ show p)
        PP.<$>
        PP.textStrict "Operators:" PP.<+> PP.pretty ops
      )

    EmptyDecl ->
      PP.textStrict "Empty Item"


-- Foreign ------------------------------------------------------------------
instance PP.Pretty Foreign where
  pretty = \case
    ForeignImport ft fn hn ty ->
        PP.textStrict "Foreign Import:"
        PP.<$>
        PP.indent 2
          ( PP.textStrict "Foreign Type:" PP.<+> PP.pretty ft
            PP.<$>
            PP.textStrict "Foreign Name:" PP.<+> PP.pretty fn
            PP.<$>
            PP.textStrict "Hawk Name:" PP.<+> PP.pretty hn
            PP.<$>
            PP.textStrict "Type Sig:" PP.<+> PP.pretty ty
          )

    ForeignExport ft hn ->
      PP.textStrict "Foreign Export:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "Foreign Type:" PP.<+> PP.pretty ft
          PP.<$>
          PP.textStrict "Hawk Name:" PP.<+> PP.pretty hn
        )

instance PP.Pretty ForeignType where
  pretty ForeignC =
    PP.textStrict "ForeignC"