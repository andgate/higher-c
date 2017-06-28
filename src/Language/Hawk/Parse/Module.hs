{-# LANGUAGE  OverloadedStrings
            , FlexibleContexts
            , ConstraintKinds
            , TypeFamilies
  #-}
module Language.Hawk.Parse.Module where

import Control.Applicative
import Control.Lens
import Data.Text (Text)
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax


import qualified Text.Megaparsec.Prim       as P
import qualified Text.Megaparsec.Combinator as P



moduleP :: MonadParser m => FilePath -> m [Token]
moduleP fp =
    many anyT
    --mkModPs fp <$> modHeader <*> modBody


modHeader :: MonadParser m => m [Text]
modHeader =
  linefold $ rsvp "mod" >> modPathP





modPathP :: MonadParser m => m [Text]
modPathP = return ["Example"]


modBody :: MonadParser m => m [[Token]]
modBody = return []