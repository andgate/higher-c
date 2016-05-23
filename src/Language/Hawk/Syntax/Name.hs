module Language.Hawk.Syntax.Name where

import Data.Binary
import qualified Data.Maybe as Maybe

import qualified Language.Hawk.Syntax.Helpers as Help
import qualified Language.Hawk.Syntax.ModuleName as ModuleName

-- -----------------------------------------------------------------------------
-- | Name

type Source
  = Raw
  
type Valid
  = Raw
  
type Canonical
  = Name Home
  
type Typed
  = Name Home


type Raw = String

data Name h
  = Name
    { home :: h
    , name :: Raw
    }
    deriving (Eq, Ord)

-- | Canonical Home
data Home
  = BuiltIn
  | Module ModuleName.Name
  | Local
  deriving (Eq, Ord)

    
-- -----------------------------------------------------------------------------
-- Name helpers

local :: Raw -> Name Home
local =
  Name Local
  
  
builtin :: Raw -> Name Home
builtin =
  Name BuiltIn
  
  
fromModule :: ModuleName.Name -> Raw -> Name Home
fromModule home =
  Name (Module home)



isLocalHome :: Home -> Bool
isLocalHome home =
  case home of
    BuiltIn ->
      False
      
    Module _ ->
      False
      
      
    Local ->
      True


      
      
isLocal :: (String -> Bool) -> Name Home -> Bool
isLocal check (Name home name) =
  case home of
    Local ->
      check name
      
    _ ->
      False
      
      
-- | Name toString
class ToString a where
  toString :: a -> String
    

instance (ToString h) => ToString (Name h) where
  toString (Name home name) =
    toString home ++ name
    
instance ToString Home where
  toString home =
    case home of
      BuiltIn ->
        ""
        
      Module moduleName ->
        toString moduleName ++ "."
        
      Local ->
        ""
        

instance ToString (ModuleName.Name) where
  toString = ModuleName.toString
  
    
 
    
instance (Binary h) => Binary (Name h) where
  put (Name name home) =
    put name >> put home
        
  get =
    Name <$> get <*> get



instance Binary Home where
  put home =
    case home of
      BuiltIn ->
        putWord8 0
        
      Module path ->
        putWord8 1 >> put path
        
      Local ->
        putWord8 2
        
  get =
    do  tag <- getWord8
        case tag of
          0 -> return BuiltIn
          1 -> Module <$> get
          2 -> return Local
          _ -> error "Unexpected tag when deserializing name home"