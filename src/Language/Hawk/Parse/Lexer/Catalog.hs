module Language.Hawk.Parse.Lexer.Catalog where

import Conduit
import Language.Hawk.Parse.Lexer.Token (Token)


-- | Catalogs tokens into items.
-- Items are seperated by linefolds
catalog :: Monad m => Conduit Token m [Token]
catalog = undefined