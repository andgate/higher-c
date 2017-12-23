module Language.Hawk.Syntax.Definition.Source where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Language.Hawk.Syntax.Pattern.Source

import qualified Text.PrettyPrint.Leijen.Text as PP

data Def term
  = Def 
      { defName :: Text
      , defArgs :: NonEmpty (Clause term) 
      , defTerm :: Maybe term
      }
    deriving (Show)


data Clause term = Clause [Pat term Text] term
  deriving (Show)

instance (PP.Pretty term) => PP.Pretty (Def term) where
  pretty = undefined