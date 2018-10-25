{-# LANGUAGE  TemplateHaskell
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.TypeCheck.Error where

import Control.Lens
import Data.Text (Text)
import Language.Hawk.Syntax.Type (Type)
import Language.Hawk.Syntax.Location
import Language.Hawk.TypeCheck.Constraint
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Text.PrettyPrint.Leijen.Text as P


data TcErr
  = UnificationFail Type Type
  | UnificationKindsMismatch Type Type
  | UnificationMismatch [Type] [Type]
  | InfiniteType Text Type
  | UnboundVariables [Text]
  | AmbiguousType [Constraint]
  | UndefinedConstructor Text Loc
  deriving(Show)

makeClassyPrisms ''TcErr

instance Pretty TcErr where
    pretty = \case
      UnificationFail have want ->
        P.textStrict "Unification fail:"
          P.<$>
          P.indent 2
          ( P.textStrict "have:" P.<+> pretty have
            P.<$>
            P.textStrict "want:" P.<+> pretty want
          )
      
      UnificationKindsMismatch have want ->
        P.textStrict "Kind mismatch:"
          P.<$>
          P.indent 2
          ( P.textStrict "have:" P.<+> pretty have
            P.<$>
            P.textStrict "want:" P.<+> pretty want
          )
      
      UnificationMismatch have want ->
        P.textStrict "Type mismatch:"
        P.<$>
        P.indent 2 
        ( P.textStrict "actual:" P.<+> pretty have
          P.<$>
          P.textStrict "expected:" P.<+> pretty want
        )

      InfiniteType n t ->
        P.textStrict "Infinite type undefined"

      UnboundVariables vs ->
        P.textStrict "Unbound variables encountered:" P.<+> pretty vs

      AmbiguousType cs ->
        P.textStrict "ambigiuous type??"

      UndefinedConstructor n l ->
        P.pretty l P.<+> P.textStrict "Undefined constructor encountered" P.<+> P.squotes (P.textStrict n)
        
