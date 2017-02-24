{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Language.Hawk.Syntax.Name where

import Data.Aeson ((.=))
import Data.Binary
import Data.Data
import Data.Maybe (isJust)
import Data.Text.Lazy (Text)
import Data.Tree
import Data.Typeable

import qualified Data.Aeson                       as Json
import qualified Data.Maybe                       as Maybe
import qualified Data.Text.Lazy                   as Text
import qualified Language.Hawk.Report.Region      as R
import qualified Text.PrettyPrint.ANSI.Leijen     as PP

-- -----------------------------------------------------------------------------
-- | Name

type Source
  = Name
  
type Valid
  = QName
  
type Typed
  = QName

type RName = Text

type Paths = Tree Name
type Path = [Name]

type Home = Maybe R.Position

data Name
  = Name RName Home
    deriving (Eq, Ord, Show, Data, Typeable)

data QName
  = QName RName Path Home
    deriving (Eq, Ord, Show, Data, Typeable)
    

data Package =
  Package
    { version :: Version
    , author :: Text
    , name :: Text
    } deriving (Eq, Ord, Show, Data, Typeable)


-- | Package Version
data Version
  = Version
    { _major :: Int
    , _minor :: Int
    , _patch :: Int
    } deriving (Eq, Ord, Show, Data, Typeable)


-- -----------------------------------------------------------------------------
-- Name helpers


empty :: Name
empty = Name "" Nothing


exLocal :: Name -> Text
exLocal (Name t _) = t


local :: R.Position -> Text -> Name
local p n =
  Name n (Just p)
  
  
builtin :: Text -> Name
builtin n =
  Name n Nothing
  
builtinQ :: Text -> QName
builtinQ n =
  QName n [] Nothing


isLocalHome :: Home -> Bool
isLocalHome = isJust
      
      
-- | Name toString
class ToString a where
  toString :: a -> String
    

instance ToString Paths where
  toString =
    drawTree . fmap toString

instance ToString Name where
  toString (Name n (Just (R.Position l c))) =
    Text.unpack n ++ " @ " ++ show l ++ ":" ++ show c
  toString (Name n Nothing) =
    Text.unpack n
    
instance ToString Home where
    toString h =
      case h of
        Nothing ->
          "Builtin"
          
        Just (R.Position r c) ->
          show r ++ ":" ++ show c


instance PP.Pretty Name where
    pretty n =
      PP.string (toString n)

    
instance Binary Name where
    put (Name h n) =
      put h >> put n
          
    get =
      Name <$> get <*> get