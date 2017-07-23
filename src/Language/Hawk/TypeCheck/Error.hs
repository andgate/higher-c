{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.TypeCheck.Error where

import Control.Lens
import Language.Hawk.Syntax (Type, Tyvar, Var, Con)
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as P


data TcErr
  = UnificationFailure Type (Maybe Location) Type (Maybe Location)
  | OccursCheckFail Tyvar (Maybe Location) Type (Maybe Location)
  | UnboundVariable Var Location
  | UnboundConstructor Con Location
  deriving(Show)

makeClassyPrisms ''TcErr

instance Pretty TcErr where
    pretty = \case
      UnificationFailure t1 l1 t2 l2 ->
        P.textStrict "Types do not unify:"
        P.<$>
        P.indent 2 
        ( maybe (pretty t1)
                (\l1' -> pretty l1' P.<+> pretty t1)
                l1
          P.<$>
          P.textStrict "vs."
          P.<$>
          maybe (pretty t2)
                (\l2' -> pretty l2' P.<+> pretty t2)
                l2
        )

      OccursCheckFail v lv t lt ->
        P.textStrict "Occurs check fails:"
        P.<$>
        P.indent 2
        ( maybe (pretty v)
                (\lv' -> pretty lv' P.<+> pretty v)
                lv
          P.<$> 
          P.textStrict "vs."
          P.<$>
          maybe (pretty t)
                (\lt' -> pretty lt' P.<+> pretty t)
                lt
        )

      UnboundVariable v loc ->
        P.pretty loc P.<+>
        P.textStrict "Unbound variable encountered:" P.<+> pretty v

      UnboundConstructor c loc ->
        P.pretty loc P.<+>
        P.textStrict "Unbound constructor encountered:" P.<+> pretty c