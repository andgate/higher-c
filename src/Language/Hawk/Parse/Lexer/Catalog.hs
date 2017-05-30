module Language.Hawk.Parse.Lexer.Catalog where

import Conduit
import Control.Monad.State.Strict (StateT, get, put, modify)
import Language.Hawk.Parse.Lexer.Token


-- | Catalogs tokens into items.
-- Items are seperated by linefolds
-- Note: This discards Eof tokens
catalog :: Monad m => Conduit Token m [Token]
catalog = go [] 0
  where
    go ts depth = do
      mayt <- await
      case mayt of
        Just t 
          | isTokClass TokenLn t -> 
              go (t:ts) (depth+1)

          | isTokClass TokenLn' t ->
              if (depth-1) == 0
                then do 
                  yield $ reverse (t:ts)
                  go [] 0
              else
                go (t:ts) (depth-1)
                  
          | isTokClass TokenEof t ->
              go ts depth -- discards EOF

          | otherwise ->
              go (t:ts) depth
              
        Nothing -> return ()  

      