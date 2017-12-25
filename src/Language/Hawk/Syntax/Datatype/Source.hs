{-# LANGUAGE  OverloadedStrings
            , LambdaCase
            , TupleSections
            , DeriveFoldable
            , DeriveFunctor
            , DeriveTraversable
  #-}
module Language.Hawk.Syntax.Datatype.Source where

import Bound
import Bound.Scope
import Bound.Var
import Data.Text (Text)
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Signature
import Language.Hawk.Syntax.Telescope

import qualified Text.PrettyPrint.Leijen.Text as PP


------------------------------------------------------------------------
-- Type Structure
data Datatype t v =
  Datatype
    { _dtName :: Text
    , _dtArgs :: [(Text, t v)]
    , _dtCons  :: [Constr (t v)]
    }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


data Constr term
  = Constr { _constrName :: Text
           , _constrType :: term
           }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


-------------------------------------------------------------------------
-- Helper Instances


-- Pretty Printing
instance (PP.Pretty (t v)) => PP.Pretty (Datatype t v) where
    pretty (Datatype n vs cs) =
      undefined

instance (PP.Pretty t) => PP.Pretty (Constr t) where
    pretty (Constr n t) =
      PP.pretty n PP.<+> PP.textStrict ":" PP.<+> PP.pretty t


-------------------------------------------------------------------------
-- Helpers

-- Get term level names, such as constructors and record labels
--dataNames :: DataS t v -> [Text]
--dataNames (DataS cs) = map _constrName b 


-- Get type signatures introduces by struct
--structSigs :: DataS t v -> [Sig (t v)]
--structSigs (DataS cs) = map constrSig cs

constrSig :: Constr t -> Sig t
constrSig (Constr n t) = Sig n t