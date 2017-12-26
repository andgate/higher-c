{-# Language  KindSignatures
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.Syntax.Definition.Source where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Language.Hawk.Syntax.Class
import Language.Hawk.Syntax.Datatype.Source
import Language.Hawk.Syntax.Fixity
import Language.Hawk.Syntax.Foreign
import Language.Hawk.Syntax.Pattern.Source

import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- Foreign Rules

data Lib (t :: * -> *) (v :: *) = Lib
  { _defs     :: Map Text (Def (t v))
  , _sigs     :: Map Text (t v)
  , _datas    :: Map Text (Datatype t v)
  , _foreigns :: Map Text (Foreign (t v))
  , _fixities :: [Fixity]
  } deriving (Show)

data Def (t :: *) = Def
  { defName    :: Text
  , defClauses :: NonEmpty (Clause t) 
  , defType    :: Maybe t
  } deriving (Show)

data Clause term
  = Clause [Pat term Text] term
  deriving (Show)


-- -----------------------------------------------------------------------------
-- Pretty Printing

instance (PP.Pretty term) => PP.Pretty (Def term) where
  pretty = undefined


-- -----------------------------------------------------------------------------
-- Foreign Rules

fromDef :: Def (t v) -> Lib t v
fromDef = undefined

fromSig :: (Text, t v) -> Lib t v
fromSig = undefined

fromDatatype :: Datatype t b -> Lib t v
fromDatatype = undefined

fromForeign :: Foreign (t v) -> Lib t v
fromForeign = \case
  _ -> undefined

fromFixity :: Fixity -> Lib t v
fromFixity = undefined
