{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.TypeCheck.Error where

import Control.Lens
import Language.Hawk.Syntax (Type, TVar, Var)
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (Pretty(..), (<>), (<+>))

import qualified Text.PrettyPrint.Leijen.Text as P


data TcErr
  = UnificationFailure Type Type
  | OccursCheckFail TVar Type
  | UnboundVariable Var Location
  | UnboundConstructor Var Location
  deriving(Show)

makeClassyPrisms ''TcErr

instance Pretty TcErr where
    pretty = \case
      UnificationFailure t1 t2 ->
        P.textStrict "Types do not unify:"
          P.<$> P.indent 2 (pretty t1 <+> "vs." <+> pretty t2)

      OccursCheckFail v t ->
        P.textStrict "Occurs check fails:"
          P.<$> P.indent 2 (pretty v <+> "vs." <+> pretty t)

      UnboundVariable v loc ->
        P.textStrict "Unbound variable encountered:"
          <+> pretty v

      UnboundConstructor c loc ->
        P.textStrict "Unbound constructor encountered:"
          <+> pretty c
          P.<$> P.indent 2 (P.textStrict "in" <+> pretty loc)