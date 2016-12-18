{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Hawk.Parse.Layout where

import Control.Applicative
import Control.Arrow
import Control.Lens (Simple, Lens, over, (^.))
import Control.Monad.State
import Data.Int
import Safe
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta


data Layout
  = Layout
    { layoutColumn :: {-# UNPACK #-} !Int64 }
  deriving (Show)


isLayoutInvalid :: Layout -> Int64 -> Bool
isLayoutInvalid layout col =
  layoutColumn layout >= col

        
isInLayout :: Layout -> Int64 -> Bool
isInLayout layout col =
  layoutColumn layout < col



data LayoutEnv =
  LayoutEnv {
    layoutStack :: [Layout],
    lastComment :: String
  }
  deriving (Show)


defLayoutEnv :: LayoutEnv
defLayoutEnv = LayoutEnv [] ""
  
class HasLayoutEnv u where
    layoutEnv :: Simple Lens u LayoutEnv

instance HasLayoutEnv LayoutEnv where
    layoutEnv = id
    

{-  
instance (HasLayoutEnv s) => HasLayoutEnv (MonadState s m) where
    layoutEnv = get
-}

  
type LayoutState m =
  MonadState LayoutEnv m


pushLayout :: LayoutState m => Layout -> m ()
pushLayout layout = do
  LayoutEnv s c <- get
  put (LayoutEnv (layout:s) c)
  


popLayout :: LayoutState m => m Layout
popLayout = do
  layout <- peekLayout
  LayoutEnv s c <- get
  let s' = tail s
      res = LayoutEnv s' c
  put res
  return layout
  

peekLayout :: LayoutState m => m Layout
peekLayout = do
  LayoutEnv s c <- get
  return $ headDef (Layout 0) s
  


class (LayoutState m, DeltaParsing m) => LayoutParsing m where
    ws :: m ()
    ws = do 
      try spaces
      col <- column <$> position
      layout <- peekLayout
      
      if isInLayout layout col then
        return ()
      
      else  
        fail $ "Expected content after column " ++ show (layoutColumn layout)
        
    
    freshLine :: m ()
    freshLine = do
      try spaces
      col <- column <$> position
      layout <- peekLayout
      
      if layoutColumn layout == col then
        return ()
      else
        fail $ "Expected fresh line on column " ++ show (layoutColumn layout)
              
              
    startLayout :: m ()
    startLayout =
      ws >> position >>= (column >>> Layout >>> pushLayout)
        
    
    endLayout :: m ()
    endLayout = try (spaces *> eof) <|> do
      spaces
      col <- column <$> position
      layout <- popLayout
      
      if isLayoutInvalid layout col then
        return ()
        
      else  
        fail $ "Expected layout to end on column " ++ show (layoutColumn layout)



instance (DeltaParsing m) => LayoutParsing (StateT LayoutEnv m)
  
  
withLayout :: LayoutParsing m => m a -> m a
withLayout p =
  startLayout *> p <* endLayout


lpad :: LayoutParsing m => m a -> m a
lpad p =
  ws *> p

pad :: LayoutParsing m => m a -> m a
pad p =
  ws *> p <* ws