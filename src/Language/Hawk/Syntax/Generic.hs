{-# LANGUAGE DeriveDataTypeable
           , FlexibleInstances
           , OverloadedStrings
  #-}
module Language.Hawk.Syntax.Generic where

import Data.Binary
import qualified Data.Data as D
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Tree
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen ((<+>))

import qualified Data.Text                        as Text
import qualified Language.Hawk.Report.Region      as R
import qualified Text.PrettyPrint.ANSI.Leijen     as PP


-- -----------------------------------------------------------------------------
-- | Item

data Item n e t
  = DepItem Dependency

  | SigItem (TypeSig n t)
  | FunItem (Fun n e t)
  | VarItem (Var n e t)
  
  | AliasItem (TypeAlias n t)
  | NewTypeItem (NewType n t)
  | DataItem (DataType n t)
  
  | ClassItem (TypeClass n e t)
  | InstItem (TypeClassInst n e t)
  
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Dependency

data Dependency =
  Dep
    { isQual   :: Bool
    , depPath  :: DepPath
    , depAlias :: Maybe Text
    } deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Dependency Path

data DepPath = 
    DepModule  Text DepPath
  | DepTarget  Text
  | DepTargets Bool [DepPath]
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Literal

data Literal
  = IntNum Integer
  | FloatNum Double
  | Chr Char
  | Str String
  | Boolean Bool
  deriving (Show, Eq, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Name

type RName = Text

type PathTree = Tree Name
type Paths = [Path]
type Path = [Name]

type Home = Maybe R.Region

data Name
  = Name RName Home
    deriving (Eq, Ord, Show, D.Data, Typeable)

data QName
  = QName RName RName Home
    deriving (Eq, Ord, Show, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Type

data Type n
  = TypeApp (Type n) [Type n]
  | TypeCon n
  deriving (Eq, Show, Ord, D.Data, Typeable)
-- Type variables will require at least monomorphization,
-- and won't be possible for a while.
--  | Var TVar a

type TVar = String

data QType t
  = QType (Context t) t
  deriving (Eq, Show, Ord, D.Data, Typeable)

-- Types in context are validated when parsed
-- Comma seperated list of TypeClass instances,
-- which define constraints for type variables
newtype Context t
  = Context [t]
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Expression

data Expr n t
  = ExprLit Literal
  | ExprVar n
  | ExprCon n
  
  | ExprLam [n] (Expr n t)
  
  | ExprApp (Expr n t) [Expr n t]
  | ExprLet n (Expr n t) (Expr n t)
  -- Probably need to add suport for operators later
  
  | ExprIf [Expr n t] (Expr n t)
  
  -- Specific stucture access
  | ExprAccess (Expr n t) (Expr n t)
  | ExprRefAccess (Expr n t) (Expr n t)
  
  | ExprCast (Expr n t) t
  
  | ExprBottom
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Statement

data Stmt n e t
  = StmtCall n [e]
  | StmtVar (Var n e t)
  | StmtFun (Fun n e t)
  | StmtSig (TypeSig n t)
  | StmtAssign n (Either [Stmt n e t] e)
  | StmtIf e (Stmt n e t) (Maybe (Stmt n e t))
  | StmtReturn e
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Type Signature

data TypeSig n t
  = TypeSig
    { _tySigName :: n
    , _tySigBody :: t
    }
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Variable

data Var n e t
  = Var
    { _varName  :: n
    , _varBody  :: Maybe (Either [Stmt n e t] e)
    }
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Function

data Fun n e t
  = Fun
    { _funName   :: n
    , _funParams :: [n]
    , _funBody   :: Maybe (Either [Stmt n e t] e)
    }
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | New Type

data NewType n t
  = NewType
    { _newTyName     :: n
    , _newTyVars     :: [n]
    , _newTyNewBody  :: t
    }
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Type Alias

data TypeAlias n t
    = TypeAlias
      { _tyAliasName   :: n
      , _tyAliasTyVars :: [n]
      , _tyAliasBody   :: t
      }
    deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Type Class

data TypeClass n e t
    = TypeClass 
      { _tyClassContext :: Context t
      , _tyClassName :: n
      , _tyClassVars :: [n]
      , _tyClassBody :: [Either (Fun n e t) (TypeSig n t)]
      }
    deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Type Class Instance

data TypeClassInst n e t
    = TypeClassInst 
      { _tyClassInstContext :: QType t
      , _tyClassInstName :: n
      , _tyClassInstArgs :: [t]
      , _tyClassInstBody :: [Fun n e t]
      }
    deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Data Type

data DataType n t
    = DataType
      { _dataTyName :: n
      , _dataTyVars :: [n]
      , _dataTyBody :: [TypeSig n t]
      }
    deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Pretty Printing Instances

instance (PP.Pretty l, PP.Pretty r) => PP.Pretty (Either l r) where
  pretty (Left l) =
    PP.pretty l

  pretty (Right r) =
    PP.pretty r 

-- Item ------------------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Item n e t) where
    pretty (DepItem i) =
      PP.pretty i
        
    pretty (SigItem i) =
      PP.pretty i

    pretty (FunItem i) =
      PP.pretty i

    pretty (VarItem i) =
      PP.pretty i

    pretty (AliasItem i) =
      PP.pretty i
        
    pretty (NewTypeItem i) =
      PP.pretty i

    pretty (DataItem i) =
      PP.pretty i

    pretty (ClassItem i) =
      PP.pretty i

    pretty (InstItem i) =
      PP.pretty i


-- Dependency ------------------------------------------------------------------
instance PP.Pretty Dependency where
    pretty (Dep ql p a) =
      PP.text "Import:"
      PP.<$>
      PP.indent 2
        ( PP.text "Is Qualified:" PP.<+> PP.pretty ql
          PP.<$>
          PP.text "Path:" PP.<+> PP.pretty p
          PP.<$>
          PP.text "Alias:" PP.<+> PP.text (show (Text.unpack <$> a))
        )


-- Dependency Path ---------------------------------------------------------------
instance PP.Pretty DepPath where
    pretty (DepModule n r) =
      PP.text (Text.unpack n) PP.<> PP.text "."  PP.<> PP.pretty r
        
    pretty (DepTarget n) =
      PP.text (Text.unpack n)
        
    pretty (DepTargets False rs) =
      PP.text "(" PP.<> PP.pretty rs PP.<> PP.text ")"

    pretty (DepTargets True rs) =
      PP.text "(\\" PP.<> PP.pretty rs PP.<> PP.text ")"


-- Literal ---------------------------------------------------------------------
instance PP.Pretty Literal where
  pretty literal =
    case literal of
      IntNum v ->
         PP.string "Literal Int:" <+> PP.string (show v)
         
      FloatNum v ->
        PP.string "Literal Float:" <+> PP.string (show v)
      
      Chr v ->
        PP.string "Literal Char:" <+> PP.string (show v)
      
      Str v ->
        PP.string "Literal String:" <+> PP.string (show v)
      
      Boolean v ->
        PP.string "Literal Bool:" <+> PP.string (show v)

-- Name ------------------------------------------------------------------------
instance PP.Pretty Name where
    pretty n =
      PP.string (toString n)


-- Type ------------------------------------------------------------------------
instance (PP.Pretty n) => PP.Pretty (Type n) where
  pretty (TypeApp con args) =
    PP.text "Type App:"
    PP.<$>
    PP.indent 2
      ( PP.text "con:" <+> PP.pretty con
        PP.<$>
        PP.text "args:" PP.<$> PP.indent 2 (PP.pretty args)
      )
    
  pretty (TypeCon name) =
    PP.text "Type Con" <+> PP.dquotes (PP.pretty name)


instance (PP.Pretty t) => PP.Pretty (QType t) where
    pretty (QType ctx tipe) =
      PP.text "Qualified Type:"
      PP.<$>
      PP.indent 2
        ( PP.text "context:" <+> PP.pretty ctx
          PP.<$>
          PP.text "type:" PP.<$> PP.pretty tipe
        )
        

instance (PP.Pretty t) => PP.Pretty (Context t) where
    pretty (Context assrts) =
      PP.text "Context:"
      PP.<$>
      PP.indent 2
        ( PP.text "Assertions:" <+> PP.pretty assrts
        )


-- Expr -------------------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (Expr n t ) where
  pretty (ExprLit lit) =
    PP.text "Literal Expression:"
    PP.<$>
    PP.indent 2 ( PP.pretty lit )
    
  pretty (ExprVar name) =
    PP.text "Variable Expression:"
    PP.<$>
    PP.indent 2 ( PP.pretty name )
    
  pretty (ExprCon name) =
    PP.text "Constructor Expression:"
    PP.<$>
    PP.indent 2 ( PP.pretty name )
    
  pretty (ExprApp f xs) =
    PP.text "Application Expression:"
    PP.<$>
    PP.indent 2 
        ( PP.pretty f PP.<$> PP.pretty xs )
        
  pretty (ExprLet name value definition) =
    PP.text "Let Expression:"
    PP.<$>
    PP.indent 2
      ( PP.string "name:" <+> PP.pretty name
        PP.<$>
        PP.string "value:" <+> PP.pretty value
        PP.<$> 
        PP.string "definition:" <+> PP.pretty definition
      )
      
  pretty (ExprCast e t) =
    PP.text "Cast Expression:"
    PP.<$>
    PP.indent 2
      ( PP.string "expression:" <+> PP.pretty e
        PP.<$>
        PP.string "type:" <+> PP.pretty t
      )


-- Statement -------------------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Stmt n e t ) where
  pretty (StmtCall name args) =
    PP.text "Call Statement:"
    PP.<$>
    PP.indent 2
      ( PP.string "name:" <+> PP.pretty name
        PP.<$>
        PP.string "args:" <+> PP.pretty args
      )
    
  pretty (StmtVar v) =
    PP.text "Variable Declaration Statement:"
    PP.<$>
    PP.indent 2 ( PP.pretty v )

  pretty (StmtFun f) =
    PP.text "Function Declaration Statement:"
    PP.<$>
    PP.indent 2 ( PP.pretty f )

  pretty (StmtSig s) =
    PP.text "Type Signature Declaration Statement:"
    PP.<$>
    PP.indent 2 ( PP.pretty s )

  pretty (StmtAssign assignee assignment) =
    PP.text "Assignment Statement:"
    PP.<$>
    PP.indent 2
      ( PP.string "assignee:" <+> PP.pretty assignee
        PP.<$>
        PP.string "assignment:" <+> PP.pretty assignment
      )

  pretty (StmtIf pred thenStmt elseStmt) =
    PP.text "If Statement:"
    PP.<$>
    PP.indent 2
      ( PP.string "predicate:" <+> PP.pretty pred
        PP.<$>
        PP.string "then stmt:" <+> PP.pretty thenStmt
        PP.<$>
        PP.string "else stmt:" <+> PP.pretty elseStmt
      )
    
  pretty (StmtReturn exp) =
    PP.text "Return Expression:"
    PP.<$>
    PP.indent 2 ( PP.pretty exp )


-- Type Signature ---------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (TypeSig n t) where
    pretty (TypeSig name body) =
      PP.text "Type Signature:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" <+> PP.pretty name
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- Variable ----------------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Var n e t) where
  pretty (Var name Nothing) =
    PP.text "Variable Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
      )

  pretty (Var name (Just (Left exp))) =
    PP.text "Variable Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "body:" <+> PP.pretty exp
      )

  pretty (Var name (Just (Right stmtblk))) =
    PP.text "Variable Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "body:" <+> PP.pretty stmtblk
      )


-- Function ---------------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Fun n e t) where
  pretty (Fun name params body) =
    PP.text "Function Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "params:" <+> PP.pretty params
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )


-- New Type ----------------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (NewType n t) where
    pretty (NewType name tyvars body) =
      PP.text "New Type:"
      PP.<$>
      PP.indent 2
        ( 
          PP.text "name:" <+> PP.pretty name
          PP.<$>
          PP.text "tyvars:" <+> PP.pretty tyvars
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )

-- Type Alias ---------------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (TypeAlias n t) where
    pretty (TypeAlias name tyvars body) =
      PP.text "Type Alias:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" <+> PP.pretty name
          PP.<$>
          PP.text "tyvars:" <+> PP.pretty tyvars
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- Type Class ---------------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (TypeClass n e t) where
    pretty (TypeClass ctx name tyvars body) =
      PP.text "Type Class"
      PP.<$>
      PP.indent 2
        ( PP.text "context:" <+> PP.pretty ctx
          PP.<$>
          PP.text "name:" <+> PP.pretty name
          PP.<$>
          PP.text "type vars:" <+> PP.pretty tyvars
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- Type Class Instance --------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (TypeClassInst n e t) where
    pretty (TypeClassInst ctx name args body) =
      PP.text "Type Class Instance:"
      PP.<$>
      PP.indent 2
        ( PP.text "context:" <+> PP.pretty ctx
          PP.<$>
          PP.text "name:" <+> PP.pretty name
          PP.<$>
          PP.text "args:" <+> PP.pretty args
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- Data Type -----------------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (DataType n t) where
    pretty (DataType name tyvars body) =
      PP.text "Data Type:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" <+> PP.pretty name
          PP.<$>
          PP.text "tyvars:" <+> PP.pretty tyvars
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- -----------------------------------------------------------------------------
-- | Binary Instances

-- Dependency Path ---------------------------------------------------------------
instance Binary DepPath where
  get = do
    n <- getWord8
    case n of
      1 -> DepModule  <$> get <*> get
      2 -> DepTarget  <$> get
      3 -> DepTargets <$> get <*> get
      
  put d =
    case d of
      DepModule n p     -> putWord8 1 >> put n >> put p
      DepTarget n       -> putWord8 2 >> put n
      DepTargets hq ns  -> putWord8 3 >> put hq >> put ns

-- Literal ---------------------------------------------------------------------
instance Binary Literal where
  get = do
    n <- getWord8
    case n of
      1 -> IntNum <$> get
      2 -> FloatNum <$> get
      3 -> Chr <$> get
      4 -> Str <$> get
      5 -> Boolean <$> get
      _ -> error "unexpected input"

  put literal =
    case literal of
      IntNum v    -> putWord8 1 >> put v
      FloatNum v  -> putWord8 2 >> put v
      Chr v       -> putWord8 3 >> put v
      Str v       -> putWord8 4 >> put v
      Boolean v   -> putWord8 5 >> put v


-- Name ------------------------------------------------------------------------
instance Binary Name where
    put (Name h n) =
      put h >> put n
          
    get =
      Name <$> get <*> get


-- Type ------------------------------------------------------------------------
instance (Binary n) => Binary (Type n) where
  put tipe =
    case tipe of
      TypeApp t1 t2 ->
        putWord8 0 >> put t1 >> put t2
        
      TypeCon name ->
        putWord8 1 >> put name
        
      --Var name ->
      --  putWord8 2 >> put name
        
  get =
    do  n <- getWord8
        case n of
          0 -> TypeApp <$> get <*> get
          1 -> TypeCon <$> get
          --2 -> Var <$> get
          _ -> error "Error reading a valid type from serialized string"


instance (Binary t) => Binary (QType t) where
  get =
    QType <$> get <*> get

  put (QType ctx tipe) =
    put ctx >> put tipe


instance (Binary t) => Binary (Context t) where
  get =
    Context <$> get

  put (Context assrts) =
    put assrts
          
-- Expr -------------------------------------------------------------------------
instance (Binary n, Binary t) => Binary (Expr n t) where
  get = do
    n <- getWord8
    case n of
      1 -> ExprLit <$> get
      2 -> ExprVar <$> get
      3 -> ExprCon <$> get
      4 -> ExprApp <$> get <*> get
      5 -> ExprLet <$> get <*> get <*> get
      6 -> ExprIf <$> get <*> get
      7 -> ExprAccess <$> get <*> get
      8 -> ExprRefAccess <$> get <*> get
      9 -> ExprCast <$> get <*> get
      
  put e =
    case e of
      ExprLit v           -> putWord8 1 >> put v
      ExprVar n           -> putWord8 2 >> put n
      ExprCon n           -> putWord8 3 >> put n
      ExprApp f a         -> putWord8 4 >> put f >> put a
      ExprLet n e1 e2     -> putWord8 5 >> put n >> put e1 >> put e2
      ExprIf ps d         -> putWord8 6 >> put ps >> put d
      ExprAccess e1 e2    -> putWord8 7 >> put e1 >> put e2
      ExprRefAccess e1 e2 -> putWord8 8 >> put e1 >> put e2
      ExprCast e1 t       -> putWord8 9 >> put e1 >> put t


-- Statement -------------------------------------------------------------------------
instance (Binary n, Binary e, Binary t) => Binary (Stmt n e t) where
  get = do
    n <- getWord8
    case n of
      1 -> StmtCall <$> get <*> get
      2 -> StmtVar <$> get
      3 -> StmtFun <$> get
      4 -> StmtSig <$> get
      5 -> StmtAssign <$> get <*> get
      6 -> StmtIf <$> get <*> get <*> get
      7 -> StmtReturn <$> get
      
  put e =
    case e of
      StmtCall callee args                -> putWord8 1 >> put callee >> put args
      StmtVar v                           -> putWord8 2 >> put v
      StmtVar f                           -> putWord8 3 >> put f
      StmtVar s                           -> putWord8 4 >> put s
      StmtAssign assignee assignment      -> putWord8 5 >> put assignee >> put assignment
      StmtIf pred ifStmt elseStmt         -> putWord8 6 >> put pred >> put ifStmt >> put elseStmt
      StmtReturn exp                      -> putWord8 7 >> put exp


-- Type Signature ---------------------------------------------------------------
instance (Binary n, Binary t) => Binary (TypeSig n t) where
  get =
    TypeSig <$> get <*> get

  put (TypeSig name body) =
    put name >> put body


-- Variable ----------------------------------------------------------------------
instance (Binary n, Binary e, Binary t) => Binary (Var n e t) where
  get =
      Var <$> get <*> get
      
  put (Var name body) =
      put name >> put body


-- Function ----------------------------------------------------------------------
instance (Binary n, Binary e, Binary t) => Binary (Fun n e t) where
  get =
      Fun <$> get <*> get <*> get
      
  put (Fun name params body) =
      put name >> put params >> put body


-- New Type ----------------------------------------------------------------------
instance (Binary n, Binary t) => Binary (NewType n t) where
  get =
    NewType <$> get <*> get <*> get

  put (NewType name tyvars body) =
    put name >> put tyvars >> put body


-- Type Alias ---------------------------------------------------------------------
instance (Binary n, Binary t) => Binary (TypeAlias n t) where
  get =
    TypeAlias <$> get <*> get <*> get

  put (TypeAlias name tyvars body) =
    put name >> put tyvars >> put body


-- Type Class ---------------------------------------------------------------------
instance (Binary n, Binary e, Binary t) => Binary (TypeClass n e t) where
  get =
    TypeClass <$> get <*> get <*> get <*> get

  put (TypeClass ctx name tyvars body) =
    put ctx >> put name >> put tyvars >> put body


-- Type Class Instance --------------------------------------------------------------
instance (Binary n, Binary e, Binary t) => Binary (TypeClassInst n e t) where
  get =
    TypeClassInst <$> get <*> get <*> get <*> get

  put (TypeClassInst ctx name args body) =
    put ctx >> put name >> put args >> put body


-- Data Type -----------------------------------------------------------------------
instance (Binary n, Binary t) => Binary (DataType n t) where
  get =
    DataType <$> get <*> get <*> get

  put (DataType name tyvars body) =
    put name >> put tyvars >> put body


-- -----------------------------------------------------------------------------
-- | Helpers

class ToString a where
  toString :: a -> String

-- Literal ---------------------------------------------------------------------
instance ToString Literal where 
  toString literal =
    case literal of
      IntNum n -> show n
      FloatNum n -> show n
      Chr c -> show c
      Str s -> s
      Boolean bool -> show bool
    

-- Name ------------------------------------------------------------------------
empty :: Name
empty = Name "" Nothing


exLocal :: Name -> Text
exLocal (Name t _) = t


local :: R.Region -> Text -> Name
local r n =
  Name n (Just r)

class HasBuiltin n where
  builtin :: Text -> n

instance HasBuiltin Name where
  builtin n =
    Name n Nothing

instance HasBuiltin QName where
  builtin n =
    QName n "" Nothing


isLocalHome :: Home -> Bool
isLocalHome = isJust
      

expandPathTrees :: [PathTree] -> Paths
expandPathTrees = concatMap expandPathTree
     
expandPathTree :: PathTree -> Paths
expandPathTree (Node n []) = [[n]]
expandPathTree (Node n ns) =
    map (n:) ns'
  where ns' = concatMap expandPathTree ns
  
      
-- | Name toString
instance ToString PathTree where
  toString =
    drawTree . fmap toString

instance ToString Paths where
  toString ps =
    show $ map toString ps
    
instance ToString Path where
  toString =
    intercalate "." . map toString

instance ToString Name where
  toString (Name n h) =
    Text.unpack n ++ " @ " ++ toString h
    
instance ToString Home where
    toString h =
      case h of
        Nothing ->
          "Builtin"
          
        Just (R.R (R.P r1 c1) (R.P r2 c2)) ->
          show r1 ++ ":" ++ show c1 ++ "-" ++ show r2 ++ ":" ++ show c2


-- Type ------------------------------------------------------------------------
-- These are built-in type constructors. The definition of type does not
-- directly contain these, for the sake of generality.

-- Arr (Type n) (Type n)
-- "_Arrow"

-- Primitive types
-- A list of different types, such as "_I32" and "_F32"

-- Ptr (Type n)
-- "_Pointer"

-- Array (Type n)
-- "_Array"

-- Vector (Type n)
-- "_Vector"

-- Tuple [Type n]
-- "_Tuple"

typeCon :: HasBuiltin n => Text -> [Type n] -> Type n
typeCon n [] = TypeCon $ builtin n
typeCon n args = TypeApp (TypeCon $ builtin n) args

apply :: Type n -> [Type n] -> Type n
apply con [] = con
apply con args = TypeApp con args

unit :: HasBuiltin n => Type n
unit = typeCon "_#_Unit_#_" []

arrow :: HasBuiltin n => [Type n] -> Type n
arrow [arg] = arg
arrow args = variadic "_#Arr_#_" args

tuple :: HasBuiltin n => [Type n] -> Type n
tuple [arg] = arg
tuple args = variadic "_#_Tuple_#_" args

variadic :: HasBuiltin n => Text -> [Type n] -> Type n
variadic n =
    TypeApp (TypeCon $ builtin n)

emptyCtx :: Context n
emptyCtx = Context []