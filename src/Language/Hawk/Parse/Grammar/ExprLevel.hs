{-# LANGUAGE RecursiveDo, RankNTypes #-}
module Language.Hawk.Parse.Grammar.ExprLevel where

import Control.Applicative
import Data.Monoid
import Language.Hawk.Parse.Helpers
import Text.Earley
import Text.Earley.Mixfix

import qualified Language.Hawk.Parse.Lexer as Lex
import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.Literal as Lit
import qualified Language.Hawk.Syntax.Name as N

exprLevel :: ExprOpTable -> Grammar r (Prod r Lex.Token Lex.Token E.Valid)
exprLevel exprOps = undefined

    