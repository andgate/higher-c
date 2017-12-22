{-# LANGUAGE  OverloadedStrings
            , TemplateHaskell
            , LambdaCase
  #-}
module Language.Hawk.ScopeCheck.Message where

import Control.Lens
import Data.Set (Set)
import Data.Text (Text)
import Language.Hawk.Syntax.Location
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.PrettyPrint.Leijen.Text as P

data ScMsg
  = ScStarted (Set Text)
  | ScFinished
  | ScHit Text Loc
  deriving(Show)

makeClassyPrisms ''ScMsg

instance Pretty ScMsg where
    pretty = \case
      ScStarted ns ->
        P.textStrict "Scope checker has"
          P.<+> P.pretty (Set.toList ns)
      
      ScFinished ->
        P.textStrict "Scope check completed."

      ScHit n l ->
        P.pretty l
          P.<> P.textStrict ": Found symbol"
          P.<+> P.dquotes (P.textStrict n)
