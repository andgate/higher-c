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


type LayoutEnv =
  [Layout]


defLayoutEnv :: LayoutEnv
defLayoutEnv = []
  
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
  modify (layout:)


popLayout :: LayoutState m => m Layout
popLayout = do
  layout <- peekLayout
  modify (tail)
  return layout
  

peekLayout :: LayoutState m => m Layout
peekLayout =
  headDef (Layout 0) <$> get
  


class (LayoutState m, DeltaParsing m) => LayoutParsing m where
    ws :: m ()
    ws = do 
      try spaces <|> pure ()
      col <- column <$> position
      layout <- peekLayout
      
      if isInLayout layout col then
        return ()
      
      else  
        unexpected "end of layout"
        
    
    freshLine :: m ()
    freshLine = do
      try spaces <|> pure ()
      col <- column <$> position
      layout <- peekLayout
      
      if layoutColumn layout == col then
        return ()
      else
        unexpected $ "expected on column " ++ show col
              
              
    startLayout :: m ()
    startLayout =
      lpad position >>= (column >>> Layout >>> pushLayout)
        
    
    endLayout :: m ()
    endLayout = try (spaces *> eof) <|> do
      spaces
      col <- column <$> position
      layout <- popLayout
      
      if isLayoutInvalid layout col then
        return ()
        
      else  
        unexpected "open layout"



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