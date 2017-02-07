{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Language.Hawk.Syntax.Name where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Data.Binary
import Data.Data
import Data.Text.Lazy (Text)
import Data.Typeable
import qualified Data.Maybe as Maybe

import qualified Data.Text.Lazy                   as Text
import qualified Language.Hawk.Syntax.ModuleName  as ModuleName
import qualified Language.Hawk.Report.Region      as R
import qualified Text.PrettyPrint.ANSI.Leijen     as PP

-- -----------------------------------------------------------------------------
-- | Name

type Source
  = Name
  
type Valid
  = Name
  
type Canonical
  = Name
  
type Typed
  = Name
  

data Name
  = Name
    { home :: Home
    , name :: Text
    }
    deriving (Eq, Ord, Show, Data, Typeable)

-- | Canonical Home
data Home
  = BuiltIn
  | Local R.Position
  deriving (Eq, Ord, Show, Data, Typeable)

    
-- -----------------------------------------------------------------------------
-- Name helpers


exLocal :: Name -> Text
exLocal (Name _ t) = t


local :: R.Position -> Text -> Name
local p n =
  Name (Local p) n
  
  
builtin :: Text -> Name
builtin =
  Name BuiltIn


isLocalHome :: Home -> Bool
isLocalHome h =
  case h of
    BuiltIn ->
      False
      
    Local _ ->
      True
      
      
-- | Name toString
class ToString a where
  toString :: a -> String
    

instance ToString Name where
  toString (Name h n) =
    Text.unpack n ++ " @ " ++ toString h
    
instance ToString Home where
    toString h =
      case h of
        BuiltIn ->
          "Builtin"
          
        Local (R.Position r c) ->
          show r ++ ":" ++ show c
        

instance ToString (ModuleName.Name) where
    toString = ModuleName.toString


instance PP.Pretty Name where
    pretty n =
      PP.string (toString n)

    
instance Binary Name where
    put (Name h n) =
      put h >> put n
          
    get =
      Name <$> get <*> get



instance Binary Home where
    put h =
      case h of
        BuiltIn ->
          putWord8 0
        Local p ->
          putWord8 1 >> put p
          
    get =
      do  tag <- getWord8
          case tag of
            0 -> return BuiltIn
            1 -> Local <$> get
            _ -> error "Unexpected tag when deserializing name home"