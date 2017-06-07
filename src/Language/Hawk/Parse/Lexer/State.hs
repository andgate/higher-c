{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Parse.Lexer.State where

import Control.Lens
import Language.Hawk.Report.Region

data LexState =
  LexState  { _lexRegion :: Region
            , _lexStartcode :: Int
            , _lexCommentDepth :: Int
            , _lexStringBuf :: String
            , _lexFilePath :: FilePath
            } deriving Show

defState :: FilePath -> LexState
defState = LexState (R (P 0 0) (P 0 0)) 0 0 ""

makeLenses ''LexState