module Language.Hawk.Syntax.Definition.Source where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Language.Hawk.Syntax.Fixity
import Language.Hawk.Syntax.Datatype.Source
import Language.Hawk.Syntax.Pattern.Source

import qualified Text.PrettyPrint.Leijen.Text as PP


data Lib term
  = DefMap { _fnDefs   :: Map Text (Fn term)
           , _dataDefs :: Map Text (Datatype term)
           , _foreignDefs :: Map Text (Text, term)
           , _fixityDefs :: [Fixity]
           }
  deriving (Show)

data Fn term
  = Fn { defName :: Text
       , defArgs :: NonEmpty (Clause term) 
       , defTerm :: Maybe term
       }
  deriving (Show)

data Clause term = Clause [Pat term Text] term
  deriving (Show)

instance (PP.Pretty term) => PP.Pretty (Fn term) where
  pretty = undefined