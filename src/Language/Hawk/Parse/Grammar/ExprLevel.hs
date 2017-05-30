{-# LANGUAGE RankNTypes #-}
module Language.Hawk.Parse.Grammar.ExprLevel where

import Control.Applicative
import Data.Monoid
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token (Token)
import Text.Earley
import Text.Earley.Mixfix

import qualified Language.Hawk.Syntax.Expression as E
import qualified Language.Hawk.Syntax.Literal as Lit
import qualified Language.Hawk.Syntax.Name as N

exprLevel :: ExprOpTable -> Grammar r (Prod r Token Token E.Valid)
exprLevel exprOps = undefined

    