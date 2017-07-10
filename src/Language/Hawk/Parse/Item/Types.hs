{-# LANGUAGE  TemplateHaskell
            , RankNTypes
  #-}
module Language.Hawk.Parse.Item.Types where

import Control.Lens
import Data.Default.Class
import Data.Map (Map)
import Language.Hawk.Syntax
import Language.Hawk.Parse.Helpers (ParserOpTable)

import qualified Text.Megaparsec.Expr as P

data GlobalInfo
  = GlobalInfo
      { _gFilePath :: FilePath
      , _gOps      :: ParserOpTable
      }

makeClassy ''GlobalInfo