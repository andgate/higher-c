{-# LANGUAGE TemplateHaskell #-}
module Language.Hawk.Lex.State where

import Control.Lens
import Language.Hawk.Syntax.Location
import Language.Hawk.Lex.Token (Token)

data LexState =
  LexState  { _lexTokAcc :: [Token]
            , _lexRegion :: Region
            , _lexStartcode :: Int
            , _lexCommentDepth :: Int
            , _lexStringBuf :: String
            , _lexFilePath :: FilePath
            } deriving Show



initialLexState :: FilePath -> LexState
initialLexState fp =
  LexState
    { _lexTokAcc = []
    , _lexRegion = R (P 0 0) (P 0 0)
    , _lexStartcode = 0
    , _lexCommentDepth = 0
    , _lexStringBuf = ""
    , _lexFilePath = fp
    }

makeLenses ''LexState

instance HasRegion LexState where
  region = lexRegion


