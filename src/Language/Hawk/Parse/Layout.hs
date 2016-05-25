{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Hawk.Parse.Layout where

import Control.Applicative
import Control.Lens (Simple, Lens, over, (^.))
import Control.Monad.State
import Data.Int
import Safe
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta


data Layout = 
    Aligned {-# UNPACK #-} !Int64
  | Floating {-# UNPACK #-} !Int64
  deriving (Show)


defaultLayout :: Layout
defaultLayout =
  Aligned 0


isLayoutInvalid :: Layout -> Int64 -> Bool
isLayoutInvalid layout currCol =
  case layout of
    Aligned col ->
        col > currCol
    
    Floating col -> 
        col >= currCol


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
  headDef defaultLayout <$> get



class (LayoutState m, DeltaParsing m) => LayoutParsing m where
    ws :: m ()
    ws = do 
      spaces
      col <- column <$> position
      layout <- peekLayout
      case isLayoutInvalid layout col of
        False -> 
            return ()
          
        True ->
            unexpected "end of layout"
              
              
    startLayout :: (Int64 -> Layout) -> m ()
    startLayout layoutCon = do
      spaces
      pushLayout . layoutCon . column =<< position
      
    
    endLayout :: m ()
    endLayout = try (spaces *> eof) <|> do
      spaces
      col <- column <$> position
      layout <- popLayout
      case isLayoutInvalid layout col of
        True -> 
            return ()
          
        False ->
            unexpected "open layout"



instance (DeltaParsing m) => LayoutParsing (StateT LayoutEnv m)
  

startAlignedLayout :: LayoutParsing m => m ()
startAlignedLayout =
  startLayout Aligned
      
    
startFloatingLayout :: LayoutParsing m => m ()
startFloatingLayout =
  startLayout Floating
  
  
withLayout :: LayoutParsing m => (Int64 -> Layout) -> m a -> m a
withLayout layoutCon p =
  startLayout layoutCon *> p <* endLayout
  

withAlignedLayout :: LayoutParsing m => m a -> m a
withAlignedLayout =
  withLayout Aligned


withFloatingLayout :: LayoutParsing m => m a -> m a
withFloatingLayout =
  withLayout Floating
