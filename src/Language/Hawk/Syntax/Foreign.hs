{-# Language DeriveGeneric, LambdaCase, OverloadedStrings #-}
module Language.Hawk.Syntax.Foreign where

import Data.Aeson
import Data.Binary
import Data.Default.Class
import Data.Text
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.Signature
import qualified Text.PrettyPrint.Leijen.Text as PP


-------------------------------------------------------------------------
-- Foreign 
data Foreign
  = ForeignImport ForeignType (L Text) (L Text) Type
  | ForeignExport ForeignType (L Text)
  deriving (Show, Eq, Generic)

instance Binary Foreign
instance FromJSON Foreign
instance ToJSON Foreign


data ForeignType =
  ForeignC
  deriving (Show, Eq, Generic)

instance Binary ForeignType
instance FromJSON ForeignType
instance ToJSON ForeignType


------------------------------------------------------------------------------
-- Pretty Printing

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


foreignSig :: Foreign -> Maybe Sig
foreignSig = \case
    ForeignImport _ _ (L _ n) t -> Just (Sig n t)
    ForeignExport _ _ -> Nothing