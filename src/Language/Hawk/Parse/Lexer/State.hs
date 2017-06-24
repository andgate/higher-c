{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Parse.Lexer.State where

import Control.Lens
import Language.Hawk.Report.Region
import Language.Hawk.Parse.Lexer.Token (Token)

data LexState =
  LexState  { _lexTokAcc :: [Token]
            , _lexRegion :: Region
            , _lexStartcode :: Int
            , _lexCommentDepth :: Int
            , _lexStringBuf :: String
            , _lexFilePath :: FilePath
            } deriving Show

defState :: FilePath -> LexState
defState = LexState [] (R (P 0 0) (P 0 0)) 0 0 ""

makeLenses ''LexState