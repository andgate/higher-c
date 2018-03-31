{-# Language LambdaCase, OverloadedStrings #-}
module Language.Hawk.Syntax.Foreign where

import Data.Aeson
import Data.Binary
import Data.Text
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Signature


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

instance (Pretty t) => Pretty (Foreign t) where
  pretty = \case
    ForeignImport ft fn hn ty ->
      vcat
        [ "Foreign Import:"
        , indent 2 $ vcat
            [ "Foreign Type:" <+> pretty ft
            , "Foreign Name:" <+> pretty fn
            , "Hawk Name:"    <+> pretty hn
            , "Type Sig:"     <+> pretty ty
            ]
        ]

    ForeignExport ft hn ->
      vcat
        [ "Foreign Export:"
        , indent 2 $ vcat
          [ "Foreign Type:" <+> pretty ft
          , "Hawk Name:"    <+> pretty hn
          ]
        ]

instance Pretty ForeignType where
  pretty ForeignC =
    "ForeignC"


foreignSig :: Foreign t -> Maybe (Sig t)
foreignSig = \case
    ForeignImport _ _ (L _ n) t -> Just (Sig n t)
    ForeignExport _ _ -> Nothing