{-# LANGUAGE  DeriveFunctor
            , DeriveFoldable
            , DeriveTraversable
            , MonadComprehensions
            , OverloadedStrings
            , GeneralizedNewtypeDeriving
            , LambdaCase
  #-}
module Language.Hawk.Syntax.Pattern.Elab where

import Bound
import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.Functor.Classes
import Data.Hashable
import Data.Monoid
import Data.Text
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Name

import qualified Text.PrettyPrint.Leijen.Text as PP


------------------------------------------------------------------------------
-- Scoped Pattern

data Pat t b
  = PVar NameHint b
  | PLit Lit
  | PCon Text [t] [(Pat t b, t)]
  | PView t (Pat t b)
  deriving(Show, Foldable, Functor, Traversable)


------------------------------------------------------------------------------
-- Pretty Printing

instance (PP.Pretty t, PP.Pretty b) => PP.Pretty (Pat t b) where
  pretty = \case
    PVar h n ->
      PP.pretty n

    PLit l ->
      PP.pretty l
    
    PCon n ps args ->
      undefined

    PView t p ->
      PP.pretty t
        PP.<> PP.textStrict "@"
        PP.<> PP.parens (PP.pretty p)

