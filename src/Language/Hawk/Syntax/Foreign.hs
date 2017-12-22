{-# Language LambdaCase, OverloadedStrings #-}
module Language.Hawk.Syntax.Foreign where

import Data.Aeson
import Data.Binary
import Data.Default.Class
import Data.Text
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Signature
import qualified Text.PrettyPrint.Leijen.Text as PP


-------------------------------------------------------------------------
-- Foreign
data Foreign t
  = ForeignImport ForeignType (L Text) (L Text) t
  | ForeignExport ForeignType (L Text)
  deriving (Show, Eq)


data ForeignType =
  ForeignC
  deriving (Show, Eq)


------------------------------------------------------------------------------
-- Pretty Printing

instance (PP.Pretty t) => PP.Pretty (Foreign t) where
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


foreignSig :: Foreign t -> Maybe (Sig t)
foreignSig = \case
    ForeignImport _ _ (L _ n) t -> Just (Sig n t)
    ForeignExport _ _ -> Nothing