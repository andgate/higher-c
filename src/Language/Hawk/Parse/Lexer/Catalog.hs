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
        Just t@(Token c _ _ _) ->
            case c of
              TokenLn -> 
                go (t:ts) (depth+1)

              TokenLn' ->
                  if (depth-1) == 0
                    then do 
                      yield $ reverse (t:ts)
                      go [] 0
                  else
                    go (t:ts) (depth-1)
                      
              TokenEof ->
                  go ts depth -- discards EOF

              _ ->
                  go (t:ts) depth
                
        Nothing -> return ()  

      