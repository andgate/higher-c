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



modP :: MonadParser m => FilePath -> m ModPs
modP fp =
  mkModPs fp <$> linefold modHeader
              <*> linefolds (modItem fp)


subModP :: MonadParser m
        => FilePath -> m ModPs
subModP fp =
    mkModPs fp <$> modHeader
               <*> subModBody fp

modHeader :: MonadParser m => m [Text]
modHeader =
  rsvp "mod" >> modPath


subModBody :: MonadParser m
           => FilePath -> m [Either ModPs [Token]]
subModBody fp =
  rsvp ":" *> (block $ modItem fp)


modItem :: MonadParser m
        => FilePath -> m (Either ModPs [Token])
modItem fp =
       P.try (Left <$> subModP fp)
  <|> (Right <$> anyLayout)


modPath :: MonadParser m => m [Text]
modPath =
  modPath' <|> (pure <$> modId)

modPath' :: MonadParser m => m [Text]
modPath' =
  (:) <$> modId <*> modPathNext

modPathNext :: MonadParser m => m [Text]
modPathNext =
  (rsvp "." *> modPath') <|> return []