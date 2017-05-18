{-# LANGUAGE RecursiveDo, RankNTypes #-}
module Language.Hawk.Parse.Grammar.TypeLevel where

import Control.Applicative
import Data.Monoid
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token (Token)
import Text.Earley
import Text.Earley.Mixfix

import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Type as Ty

typeLevel :: ExprOpTable -> Grammar r (Prod r Token Token Ty.Source)
typeLevel typOps = undefined

