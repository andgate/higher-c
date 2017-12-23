{-# LANGUAGE  OverloadedStrings
            , LambdaCase
            , TupleSections
            , DeriveFoldable
            , DeriveFunctor
            , DeriveTraversable
  #-}
module Language.Hawk.Syntax.DataS where

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
data DataS t v
    = DataS { _dsCons  :: [Constr (Scope TeleVar t v)] }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


data Constr t
    = Constr
      { _constrName :: Text
      , _constrType :: t
      }
      deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


-------------------------------------------------------------------------
-- Helper Instances


-- Pretty Printing
instance (PP.Pretty (t v)) => PP.Pretty (DataS t v) where
    pretty (DataS cs) =
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