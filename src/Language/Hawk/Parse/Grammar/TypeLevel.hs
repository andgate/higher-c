{-# LANGUAGE RecursiveDo, RankNTypes #-}
module Language.Hawk.Parse.Grammar.TypeLevel where

import Control.Applicative
import Data.Monoid
import Language.Hawk.Parse.Helpers
import Text.Earley
import Text.Earley.Mixfix

import qualified Language.Hawk.Parse.Lexer as Lex
import qualified Language.Hawk.Syntax.Name as N
import qualified Language.Hawk.Syntax.Type as Ty

typeLevel :: ExprOpTable -> Grammar r (Prod r Lex.Token Lex.Token Ty.Source)
typeLevel typOps = undefined

