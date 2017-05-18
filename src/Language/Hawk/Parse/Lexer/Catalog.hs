module Language.Hawk.Parse.Lexer.Catalog where

import Conduit
import Language.Hawk.Parse.Lexer.Token (Token)

catalog :: Monad m => Conduit Token m [Token]
catalog = undefined