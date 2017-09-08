{-# LANGUAGE  DeriveGeneric
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.Syntax.DataDecl where

import Data.Binary
import Data.Data
import Data.Text
import GHC.Generics (Generic)
import Language.Hawk.Syntax.Type

import qualified Text.PrettyPrint.Leijen.Text as PP


-- Types -----------------------------------------------------------------------
data DataDecl
    = DataDecl
      { _dataName :: Text
      , _dataBody :: [ConDecl]
      } deriving (Show, Generic)

data ConDecl
    = ConDecl Text [Type]
    | RecDecl Text [RecField]
    deriving (Show, Generic)

data RecField =
    RecField Text Type
    deriving (Show, Generic)





-- Helper Instances -----------------------------------------------------------------------
instance Binary DataDecl
instance Binary ConDecl
instance Binary RecField

instance PP.Pretty DataDecl where
    pretty (DataDecl name body) =
      PP.textStrict "Data Decl:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "name:" PP.<+> PP.pretty name
          PP.<$>
          PP.textStrict "body:" PP.<+> PP.pretty body
        )


instance PP.Pretty ConDecl where
    pretty = \case
      ConDecl n ts ->
          PP.textStrict "Constr:"
          PP.<$>
          PP.indent 2
            ( PP.textStrict "name:" PP.<+> PP.pretty n
              PP.<$>
              PP.textStrict "types:" PP.<+> PP.pretty ts
            )

      RecDecl n fs ->
          PP.textStrict "Record:"
          PP.<$>
          PP.indent 2
            ( PP.textStrict "name:" PP.<+> PP.pretty n
              PP.<$>
              PP.textStrict "fields:" PP.<+> PP.pretty fs
            )



instance PP.Pretty RecField where
    pretty (RecField n t) =
        PP.textStrict "Record Field:"
          PP.<$>
          PP.indent 2
            ( PP.textStrict "name:" PP.<+> PP.pretty n
              PP.<$>
              PP.textStrict "type:" PP.<+> PP.pretty t
            )