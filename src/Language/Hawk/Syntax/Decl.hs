{-# Language DeriveGeneric, LambdaCase, OverloadedStrings #-}
module Language.Hawk.Syntax.Decl where

import Data.Default.Class
import Data.Text
import Language.Hawk.Syntax.DataDecl
import Language.Hawk.Syntax.Expression
import Language.Hawk.Syntax.Type
import GHC.Generics (Generic)

import qualified Text.PrettyPrint.Leijen.Text as PP


data Decl
  = Sig Text Type
  | Def Text Exp
  | DataD DataDecl
  | Foreign Foreign
  | Fixity Fixity Int [Text]
  | EmptyDecl
  deriving (Show, Generic)


instance Default Decl where
  def = EmptyDecl


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
