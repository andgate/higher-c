module Language.Hawk.Docs.AST where

import qualified Data.Map as Map

import qualified Language.Hawk.Compile.Compiler.Type as Type
import qualified Language.Hawk.Report.Annotation as A


data Docs t
  = Docs
    { comment :: String
    , aliases :: Map.Map String (A.Located Alias)
    , types   :: Map.Map String (A.Located Union)
    , values  :: Map.Map String (A.Located (Value t))
    }
    
    
type Centralized =
  Docs (Maybe Type.Type)
  
type Checked =
  Docs Type.Type
  
  
data Alias
  = Alias
    { aliasComment  :: Maybe String
    , aliasArgs     :: [String]
    , aliasType     :: Type.Type
    }
    
    
data Union 
  = Union 
    { unionComment  :: Maybe String
    , unionArgs     :: [String]
    , unionCases    :: [ (String, [Type.Type]) ]
    }
    
    
data Value t
  = Value
    { valueComment    :: Maybe String
    , valueType       :: t
    , valueAssocPrec  :: Maybe (String, Int)
    }