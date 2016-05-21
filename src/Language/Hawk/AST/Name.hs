module Language.Hawk.AST.Name where

import Data.Binary
import qualified Data.Maybe as Maybe

import qualified Language.Hawk.AST.Helpers as Help
import qualified Language.Hawk.AST.ModuleName as ModuleName
import qualified Language.Hawk.Report.Region as R

-- -----------------------------------------------------------------------------
-- | Name

type Source
  = Name R.Region
  
type Valid
  = Name R.Region
  
type Canonical
  = Name (Home R.Region)
  
type Typed
  = Name (Home R.Region)


type Raw = String

data Name h
  = Name
    { home :: h
    , name :: Raw
    }
    deriving (Eq, Ord)

-- | Canonical Home
data Home a
  = BuiltIn
  | Module ModuleName.Name a
  | Local a
  deriving (Eq, Ord)

    
-- -----------------------------------------------------------------------------
-- Name helpers

raw :: a -> Raw -> Name a
raw = Name


local :: Raw -> a -> Name (Home a)
local x a =
  Name (Local a) x
  
  
builtin :: Raw -> Name (Home a)
builtin x =
  Name BuiltIn x
  
  
fromModule :: a -> ModuleName.Name -> Raw -> Name (Home a)
fromModule a home name =
  Name (Module home a) name



isLocalHome :: Home a -> Bool
isLocalHome home =
  case home of
    BuiltIn ->
      False
      
    Module _ _ ->
      False
      
      
    Local _ ->
      True


      
      
isLocal :: (String -> Bool) -> Name (Home a) -> Bool
isLocal check (Name home name) =
  case home of
    Local a ->
      check name
      
    _ ->
      False
      
      
-- | Name toString
class ToString a where
  toString :: a -> String
    

instance (ToString h) => ToString (Name h) where
  toString (Name home name) =
    toString home ++ name
    
instance ToString (Home a) where
  toString home =
    case home of
      BuiltIn ->
        ""
        
      Module moduleName _ ->
        toString moduleName ++ "."
        
      Local _ ->
        ""

instance ToString R.Region where
  toString _ = ""
        

instance ToString (ModuleName.Name) where
  toString = ModuleName.toString
  


data Listing a
  = Listing
    { _explicits :: [a]
    , _open :: Bool
    }
    deriving (Eq, Ord)
    

openListing :: Listing a
openListing =
  Listing [] True
  

closedListing :: Listing a
closedListing =
  Listing [] False
  
  
listing :: [a] -> Listing a
listing xs =
  Listing xs False
  

-- -----------------------------------------------------------------------------
-- Name Values

data Value
  = Value !String
  | Record !String !(Listing String)
  | Alias !String
  deriving (Eq, Ord)
  

getValues :: [Value] -> [String]
getValues =
  Maybe.mapMaybe getValue
  
  
getValue :: Value -> Maybe String
getValue value =
  case value of
    Value name -> Just name
    Record _ _ -> Nothing
    Alias _ -> Nothing
    
    
getAliases :: [Value] -> [String]
getAliases =
  Maybe.mapMaybe getAlias
  
  
getAlias :: Value -> Maybe String
getAlias value =
  case value of
    Value _ -> Nothing
    Record _ _ -> Nothing
    Alias name -> Just name
    

getUnions :: [Value] -> [(String, Listing String)]
getUnions =
  Maybe.mapMaybe getUnion
  
getUnion :: Value -> Maybe (String, Listing String)
getUnion value =
  case value of
    Value name -> Nothing
    Record name fields -> Just (name, fields)
    Alias _ -> Nothing
    
 
    
instance (Binary a) => Binary (Name a) where
  put (Name home name) =
    put home >> put name
        
  get =
    Name <$> get <*> get



instance (Binary a) => Binary (Home a) where
  put home =
    case home of
      BuiltIn ->
        putWord8 0
        
      Module path annot ->
        putWord8 1 >> put path >> put annot
        
      Local annot ->
        putWord8 2 >> put annot
        
  get =
    do  tag <- getWord8
        case tag of
          0 -> return BuiltIn
          1 -> Module <$> get <*> get
          2 -> Local <$> get
          _ -> error "Unexpected tag when deserializing name home"


instance Binary Value where
  put portable =
    case portable of
      Value name ->
        putWord8 0 >> put name
        
      Record name ctors ->
        putWord8 1 >> put name >> put ctors
        
      Alias name ->
        putWord8 2 >> put name
        
  get =
    do  tag <- getWord8
        case tag of
          0 -> Value <$> get
          1 -> Record <$> get <*> get
          2 -> Alias <$> get
          _ -> error "Error reading valid import/export information from serialized string"
          

instance (Binary a) => Binary (Listing a) where
  put (Listing explicits open) =
    put explicits >> put open
    
  get = Listing <$> get <*> get