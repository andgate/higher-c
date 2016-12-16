module Language.Hawk.Syntax.Module where
    

import Data.Binary
import qualified Data.Map as Map

import qualified Language.Hawk.Syntax.Items as Items
import qualified Language.Hawk.Syntax.Expression as Expression
import qualified Language.Hawk.Syntax.ModuleName as ModuleName
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Docs.AST as Docs
import qualified Language.Hawk.Compile.Package as Package
import qualified Language.Hawk.Compile.Compiler.Version as Compiler
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R

    
data Module info
  = Module
    { path :: FilePath
    , info :: info
    }
    deriving(Show)


type Source =
  Module SourceInfo
  
  
data SourceInfo
  = SourceInfo
    { srcName      :: ModuleName.Raw
    , srcImports   :: Items.Source
    , srcItems     :: Items.Source
    } deriving(Show)
  
  
type Valid =
  Module ValidInfo

data ValidInfo
  = ValidInfo
    { validName      :: ModuleName.Name
    , validImports   :: [Name.Canonical]
    , validExports   :: [Name.Canonical]
    , validItems     :: Items.Valid
    }

  
type Canonical =
  Module (Info Items.Canonical)
  
type Typed =
  Module (Info Items.Typed)


-- -----------------------------------------------------------------------------
-- Late Phase Module Information

data Info i
  = Info
    { name         :: ModuleName.Name
    , exports   :: [Name.Raw]
    , imports   :: [Name.Raw]
    , items     :: i
    , types     :: Types
    , records   :: Records
    , aliases   :: Aliases
    }

type Types =
  Map.Map String [Type.Canonical]

type Records =
  Map.Map String [(String, Type.Canonical)]
  
type Aliases  =
  Map.Map String Type.Canonical

-- -----------------------------------------------------------------------------
-- Interfaces

type Interfaces =
  Map.Map Name.Canonical Interface

data Interface
  = Interface
    { iVersion  :: Package.Version
    , iPackage  :: Package.Name
    , iExports  :: [Name.Raw]
    , iImports  :: [Name.Raw]
    , iTypes    :: Types
    , iRecords  :: Records
    , iAliases  :: Aliases
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
      , iRecords  = records myInfo
      , iAliases  = aliases myInfo
      }
      
      
instance Binary Interface where
  get =
    Interface <$> get <*> get <*> get <*> get <*> get <*> get <*> get
    
  put modul =
    do  put (iVersion modul)
        put (iPackage modul)
        put (iExports modul)
        put (iImports modul)
        put (iTypes modul)
        put (iRecords modul)
        put (iAliases modul)