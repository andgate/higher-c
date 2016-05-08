module Language.Hawk.Syntax.Module
    ( Header(..), Module(..)

    , Source, SourceInfo(..), SourceTag(..), SourceSettings, emptySettings
    , Valid, ValidInfo(..)
    , Canonical, Info(..)

    , UserImport, DefaultImport, ImportMethod(..)

    , Types
    , Aliases
    , Unions, UnionInfo, CanonicalUnion

    , Interfaces, Interface(..), toInterface
    )
    where
    

import Data.Binary
import qualified Data.Map as Map

import qualified Language.Hawk.Syntax.Item as Item
import qualified Language.Hawk.Syntax.Expression.Canonical as Canonical
import qualified Language.Hawk.Syntax.Module.Name as Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Variable as Var
import qualified Language.Hawk.Docs.AST as Docs
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Compile.Compiler.Version as Compiler
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


data Header imports
  = Header
    { _tag      :: SourceTag
    , _name     :: Name.Raw
    , _exports  :: Var.Listing (A.Located Var.Value)
    , _settings :: SourceSettings
    , _docs     :: A.Located (Maybe String)
    , _imports  :: imports
    }
    
    
    
data Module phase
  = Module
    { name :: Name.Canonical
    , path :: FilePath
    , info :: phase
    }
    
type Source =
  Module SourceInfo
  

data SourceInfo
  = Source
    { srcTag :: SourceTag
    , srcSettings :: SourceSettings
    , srcDocs :: A.Located (Maybe String)
    , srcExports :: Var.Listing (A.Located Var.Value)
    , srcImports :: [UserImport]
    , srcItems :: [Item.Source]
    }
    
data SourceTag 
  = Normal
  
  
type SourceSettings =
  A.Located [(A.Located String, A.Located String)]
  

emptySettings :: SourceSettings
emptySettings =
  A.A (error "region of empty settings should not be needed") []
  

-- | Valid Module
type Valid =
  Module ValidInfo
  
  
data ValidInfo
  = Valid
    { validDocs :: A.Located (Maybe String)
    , validExports :: Var.Listing (A.Located Var.Value)
    , validImports :: ([DefaultImport], [UserImport])
    , validItems :: Item.Valid
    }
    

-- | Canonical Module
type Canonical =
  Module (Info Canonical.Expr)
  
 
type UserImport =
  A.Located (Name.Raw, ImportMethod)
  
type DefaultImport =
  (Name.Raw, ImportMethod)
  
data ImportMethod
  = ImportMethod
    { alias :: Maybe String
    , exposedVars :: !(Var.Listing Var.Value)
    }
    
    

data Info program
  = Info
    { docs      :: A.Located (Maybe Docs.Centralized)
    , exports   :: [Var.Value]
    , imports   :: [Name.Raw]
    , program   :: program
    , types     :: Types
    , fixities  :: [Item.Infix]
    , aliases   :: Aliases
    , unions    :: Unions
    }
    

type Types =
  Map.Map String Type.Canonical
  
type Aliases =
  Map.Map String ([String], Type.Canonical)
  
type Unions =
  Map.Map String (UnionInfo String)
  

type UnionInfo v =
  ( [String], [( v, [Type.Canonical] )] )
  
type CanonicalUnion =
  (Var.Canonical, UnionInfo Var.Canonical)


type Interfaces =
  Map.Map Name.Canonical Interface

data Interface
  = Interface
    { iVersion  :: Package.Version
    , iPackage  :: Package.Name
    , iExports  :: [Var.Value]
    , iImports  :: [Name.Raw]
    , iTypes    :: Types
    , iUnions   :: Unions
    , iAliases  :: Aliases
    , iFixities :: [Item.Infix]
    }
    
    
toInterface :: Package.Name -> Canonical -> Interface
toInterface pkgName modul =
  let 
    myInfo =
      info modul
  in 
    Interface
      { iVersion  = Compiler.version
      , iPackage  = pkgName
      , iExports  = exports myInfo
      , iImports  = imports myInfo
      , iTypes    = types myInfo
      , iUnions   = unions myInfo
      , iAliases  = aliases myInfo
      , iFixities = fixities myInfo
      }
      
      
instance Binary Interface where
  get =
    Interface <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
    
  put modul =
    do  put (iVersion modul)
        put (iPackage modul)
        put (iExports modul)
        put (iImports modul)
        put (iTypes modul)
        put (iUnions modul)
        put (iAliases modul)
        put (iFixities modul)