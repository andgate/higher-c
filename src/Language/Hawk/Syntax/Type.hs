{-# LANGUAGE LambdaCase, DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}
module Language.Hawk.Syntax.Type where

import Data.Binary
import Data.Data
import Data.Default.Class
import Data.Text
import GHC.Generics (Generic)

import Language.Hawk.Syntax.Name

import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- | Type

data Type
  = TCon Con
  | TVar TVar
  | TFun Type Type
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)


data Scheme = Scheme [TVar] Type


-- -----------------------------------------------------------------------------
-- | "Smart" Constructors

tcon_ :: Text -> Type
tcon_ = TCon . Con

tfun1 :: Type -> Type -> Type
tfun1 = TFun


tfun2 :: Type -> Type -> Type -> Type
tfun2 a b = TFun a . TFun b



-- -----------------------------------------------------------------------------
-- | Instances

instance Binary Type

instance Default Type where
  def = TCon $ Con "()"


instance PP.Pretty Type where
    pretty = \case
      TCon n ->
        PP.textStrict "TCon:" PP.<+> PP.pretty n

      TVar tvar ->
        PP.pretty tvar

      TFun f x ->
        PP.textStrict "TFun:"
        PP.<$>
        PP.indent 2
          ( PP.textStrict "func:" PP.<+> PP.pretty f
            PP.<$>
            PP.textStrict "arg:" PP.<$> PP.pretty x
          )

instance PP.Pretty Scheme where
  pretty (Scheme vs t) =
    PP.textStrict "Scheme:"
    PP.<$>
    PP.indent 2
      ( PP.textStrict "tvars:" PP.<+> PP.pretty vs
        PP.<$>
        PP.textStrict "type:" PP.<$> PP.pretty t
      )