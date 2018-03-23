{-# Language  KindSignatures
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.Syntax.Definition.Source where

import Data.Default.Class
import Data.MultiMap (MultiMap)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Language.Hawk.Syntax.Class
import Language.Hawk.Syntax.Datatype.Source
import Language.Hawk.Syntax.Fixity
import Language.Hawk.Syntax.Foreign
import Language.Hawk.Syntax.Pattern.Source

import qualified Data.Map.Strict as Map
import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- Libraries, Definitions, and Clauses

data Lib (t :: * -> *) (v :: *) = Lib
  { _defs     :: Map Text [Def (t v)]
  , _sigs     :: Map Text (t v)
  , _datas    :: Map Text (Datatype t v)
  , _foreigns :: [Foreign (t v)]
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


instance Monoid (Lib t v) where
  mempty = Lib { _defs = mempty
               , _sigs = mempty
               , _datas = mempty
               , _foreigns = mempty
               , _fixities = mempty
               }

  mappend l1 l2 = Lib
    { _defs = _defs l1 <> _defs l2
    , _sigs = _sigs l1 <> _sigs l2
    , _datas = _datas l1 <> _datas l2
    , _foreigns = _foreigns l1 <> _foreigns l2
    , _fixities = _fixities l1 <> _fixities l2
    }


instance Default (Lib t v) where
  def = mempty

-- -----------------------------------------------------------------------------
-- Pretty Printing

instance (PP.Pretty var, PP.Pretty (term var)) => PP.Pretty (Lib term var) where
  pretty = undefined

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
