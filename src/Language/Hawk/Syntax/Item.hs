module Language.Hawk.Syntax.Item where

import Data.Binary

import qualified Language.Hawk.Syntax.Expression.Source as Source
import qualified Language.Hawk.Syntax.Expression.Valid as Valid
import qualified Language.Hawk.Syntax.Expression.Canonical as Canonical
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Report.Annotation as A


type Source =
  CommentOr (A.Located Raw)
  
data CommentOr a
  = Comment (A.Located String)
  | Whatever a
  

data Raw
  = Def Source.Def
  | Union (Union Type.Raw)
  | Alias (Alias Type.Raw)
  | Fixity Infix
  | Port String Type.Raw
  
  
-- Items Structure
  
data Items def tipe
  = Items
    { _defs :: [A.Commented def]
    , _unions :: [A.Commented (Union tipe)]
    , _aliases :: [A.Commented (Alias tipe)]
    , _infixes :: [Infix]
    }
    
    
type Valid = 
  Items Valid.Def Type.Raw
  
type Canonical =
  Items Canonical.Def Type.Canonical
  
  
addDef :: A.Commented d -> Items d t -> Items d t
addDef def items =
  items { _defs = def : _defs items }
  
addUnion :: A.Commented (Union t) -> Items d t -> Items d t
addUnion union items =
  items { _unions = union : _unions items }
  
addAlias :: A.Commented (Alias t) -> Items d t -> Items d t
addAlias alias items =
  items { _aliases = alias : _aliases items }
  
addInfix :: Infix -> Items d t -> Items d t
addInfix fixity items =
  items { _infixes = fixity : _infixes items }
  
  
-- Type Declarations

data Type body
  = Type
    { _name :: String
    , _args :: [String]
    , _body :: body
    }
  
type Union tipe =
  Type [(String, [tipe])]
    
type Alias tipe =
  Type tipe
  
  
data Infix
  = Infix
    { _op :: String
    , _associativity :: Assoc
    , _precedence :: Int
    }
    

data Assoc = L | N | R
  deriving (Eq)
  

assocToString :: Assoc -> String
assocToString assoc =
  case assoc of
    L -> "left"
    N -> "non"
    R -> "right"
    

instance Binary Infix where
  get =
    Infix <$> get <*> get <*> get
    
  put (Infix op assoc prec) =
    do  put op
        put assoc
        put prec
        

instance Binary Assoc where
  get =
    do  n <- getWord8
        return $ case n of
          0 -> L
          1 -> N
          2 -> R
          _ -> error "Error reading valid associativity from serialized string"
          
  put assoc =
    putWord8 $
      case assoc of
        L -> 0
        N -> 1
        R -> 2