{-# LANGUAGE FlexibleInstances
           , OverloadedStrings
  #-}
module Language.Hawk.Syntax where

import Data.Binary
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Tree
import Text.PrettyPrint.ANSI.Leijen ((<+>))

import qualified Data.Text                        as T
import qualified Language.Hawk.Report.Region      as R
import qualified Text.PrettyPrint.ANSI.Leijen     as PP


-- -----------------------------------------------------------------------------
-- | Item

data Item n
  = DepItem Dependency

  | ForeignItem (Foreign n)
  | ExposeItem (Expose n)

  | VowItem (Vow n)
  | SigItem (TypeSig n)
  | VarItem (Var n)
  | ValItem (Val n)
  | FunItem (Fun n)
  
  | NewTyItem (NewType n)
  | TyAliasItem (TypeAlias n)
  
  | TyClassItem (TypeClass n)
  | TyInstItem (TypeClassInst n)
  
  | DataItem (DataType n)
  
  deriving (Eq, Show, Ord)


type SrcItem = Item Name

-- -----------------------------------------------------------------------------
-- | Foreign

data Foreign n =
  Foreign ForeignType (TypeSig n)
  deriving (Eq, Show, Ord)

data ForeignType =
  ForeignC
  deriving (Eq, Show, Ord)

-- -----------------------------------------------------------------------------
-- | Expose

newtype Expose n =
  Expose n
  deriving (Eq, Show, Ord)

-- -----------------------------------------------------------------------------
-- | Dependency

data Dependency =
  Dep
    { isQual   :: Bool
    , depPath  :: DepPath
    , depAlias :: Maybe Text
    } deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Dependency Path

data DepPath = 
    DepModule  Text DepPath
  | DepTarget  Text
  | DepTargets Bool [DepPath]
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Literal

data Literal
  = IntNum Integer
  | FloatNum Double
  | Chr Char
  | Str String
  | Boolean Bool
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Name

type RName = Text

type PathTree = Tree Name
type Paths = [Path]
type Path = [Name]

data Home =
    Home
    { homePath :: FilePath
    , homeRegion :: R.Region
    }
  | Builtin
  deriving (Eq, Show, Ord)

data Name
  = Name 
    { _nameText :: RName
    , _nameHome :: Home
    }
    deriving (Eq, Show, Ord)

data QName
  = QName RName RName Home
    deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Type

data Type n
  = TypeFun (Type n) (Type n)
  | TypeTuple [Type n]
  | TypeApp (Type n) [Type n]
  | TypeVar n
  | TypeCon n
  deriving (Eq, Show, Ord)


data QType n
  = QType (TyContext n) (Type n)
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Type Context

newtype TyContext n
  = TyContext [TyAssert n]
  deriving (Eq, Show, Ord)


data TyAssert n
  = TyAssert n [Type n]
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Expression

data Expr n
  = ExprLit Literal
  | ExprVar n
  | ExprCon n

  | ExprAssign (Expr n) (Body n)

  | ExprLam [n] (Expr n)

  | ExprApp (Expr n) [Expr n]
  | ExprLet [LetField n] (Expr n)

  | ExprIf (Expr n) (Expr n) (Expr n)

  -- Specific stucture access
  | ExprMember (Expr n) n
  | ExprIndex (Expr n) (Expr n)

  | ExprTypeAnnot (Expr n) (Type n)
  | ExprBottom
  deriving (Eq, Show, Ord)


data LetField n
  = LetVar (Var n)
  | LetFun (Fun n)
  | LetSig (TypeSig n)
  deriving (Eq, Show, Ord)



-- -----------------------------------------------------------------------------
-- | Statement

data Stmt n
  = StmtExpr (Expr n)
  | StmtVar (Var n)
  | StmtFun (Fun n)
  | StmtSig (TypeSig n)
  | StmtIf (Expr n) [Stmt n] (Maybe [Stmt n])
  | StmtWhile (Expr n) [Stmt n]
  | StmtReturn (Expr n)
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Body

data Body n
  = BodyBlock [Stmt n]
  | BodyExpr (Expr n)
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Type Signature

data TypeSig n
  = TypeSig
    { _tySigName :: n
    , _tySigBody :: QType n
    }
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Type Signature

data Vow n
  = Vow
    { _vowName :: n
    , _vows :: [VowType]
    }
  deriving (Eq, Show, Ord)

data VowType =
  VowVar
  | VowVal
  | VowRef
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Variable

data Var n
  = Var
    { _varName  :: n
    , _varBody  :: Maybe (Body n)
    }
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Variable

data Val n
  = Val
    { _valName  :: n
    , _valBody  :: Maybe (Body n)
    }
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Function

data Fun n
  = Fun
    { _funName   :: n
    , _funParams :: [n]
    , _funBody   :: Body n
    }
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | New Type

data NewType n
  = NewType
    { _newTyName     :: n
    , _newTyVars     :: [n]
    , _newTyNewBody  :: Type n
    }
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Type Alias

data TypeAlias n
    = TypeAlias
      { _tyAliasName   :: n
      , _tyAliasTyVars :: [n]
      , _tyAliasBody   :: Type n
      }
    deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Type Class

data TypeClass n
    = TypeClass 
      { _tyClassContext :: Maybe (TyContext n)
      , _tyClassName :: n
      , _tyClassVars :: [n]
      , _tyClassBody :: [Either (Fun n) (TypeSig n)]
      }
    deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Type Class Instance

data TypeClassInst n
    = TypeClassInst 
      { _tyClassInstContext :: Maybe (TyContext n)
      , _tyClassInstName :: n
      , _tyClassInstArgs :: [Type n]
      , _tyClassInstBody :: [Fun n]
      }
    deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Data Type

data DataType n
    = DataType
      { _dataTyName :: n
      , _dataTyVars :: [n]
      , _dataTyBody :: [TypeSig n]
      }
    deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Pretty Printing Instances

instance (PP.Pretty l, PP.Pretty r) => PP.Pretty (Either l r) where
    pretty (Left l) =
      PP.pretty l

    pretty (Right r) =
      PP.pretty r 


-- Item ------------------------------------------------------------------------
instance (PP.Pretty n) => PP.Pretty (Item n) where
    pretty (DepItem i) =
      PP.pretty i
        
    pretty (SigItem i) =
      PP.pretty i

    pretty (FunItem i) =
      PP.pretty i

    pretty (VarItem i) =
      PP.pretty i
        
    pretty (NewTyItem i) =
      PP.pretty i

    pretty (TyAliasItem i) =
      PP.pretty i

    pretty (TyClassItem i) =
      PP.pretty i

    pretty (TyInstItem i) =
      PP.pretty i

    pretty (DataItem i) =
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
          PP.text "Alias:" PP.<+> PP.text (show (T.unpack <$> a))
        )


-- Dependency Path ---------------------------------------------------------------
instance PP.Pretty DepPath where
    pretty (DepModule n r) =
      PP.text (T.unpack n) PP.<> PP.text "."  PP.<> PP.pretty r
        
    pretty (DepTarget n) =
      PP.text (T.unpack n)
        
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
    pretty (TypeFun x y) =
      PP.text "Type Function:"
      PP.<$>
      PP.indent 2
        ( PP.text "from:" <+> PP.pretty x
          PP.<$>
          PP.text "to:" PP.<$> PP.pretty y
        )

    pretty (TypeTuple mems) =
      PP.text "Type Tuple:"
      PP.<$>
      PP.indent 2
        ( PP.text "members:" <+> PP.pretty mems
        )
        

    pretty (TypeApp con args) =
      PP.text "Type App:"
      PP.<$>
      PP.indent 2
        ( PP.text "con:" <+> PP.pretty con
          PP.<$>
          PP.text "args:" PP.<$> PP.pretty args
        )
      
    pretty (TypeVar name) =
      PP.text "Type Var" <+> PP.dquotes (PP.pretty name)

    pretty (TypeCon name) =
      PP.text "Type Con" <+> PP.dquotes (PP.pretty name)


instance (PP.Pretty n) => PP.Pretty (QType n) where
    pretty (QType ctx tipe) =
      PP.text "Qualified Type:"
      PP.<$>
      PP.indent 2
        ( PP.text "context:" <+> PP.pretty ctx
          PP.<$>
          PP.text "type:" <+> PP.pretty tipe
        )
        

instance (PP.Pretty n) => PP.Pretty (TyContext n) where
    pretty (TyContext assrts) =
      PP.text "Context:"
      PP.<$>
      PP.indent 2
        ( PP.text "Assertions:" <+> PP.pretty assrts
        )

instance (PP.Pretty n) => PP.Pretty (TyAssert n) where
    pretty (TyAssert con tys) =
      PP.text "Type Assertion:"
      PP.<$>
      PP.indent 2
        ( PP.text "Constructor:" <+> PP.pretty con
          PP.<$>
          PP.text "Arguments:" <+> PP.pretty tys
        )

-- Expr -------------------------------------------------------------------------
instance (PP.Pretty n) => PP.Pretty (Expr n) where
    pretty (ExprLit lit) =
      PP.text "Literal Expression:" PP.<+> PP.pretty lit
      
    pretty (ExprVar name) =
      PP.text "Variable Expression:" PP.<+> PP.pretty name
      
    pretty (ExprCon name) =
      PP.text "Constructor Expression:" PP.<+> PP.pretty name

    pretty (ExprAssign assignee rhs) =
      PP.text "Let Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "Assignee:" <+> PP.pretty assignee
          PP.<$>
          PP.string "rhs:" <+> PP.pretty rhs
        )

    pretty (ExprLam params rhs) =
      PP.text "Let Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "params:" <+> PP.pretty params
          PP.<$>
          PP.string "rhs:" <+> PP.pretty rhs
        )

    pretty (ExprApp f xs) =
      PP.text "Application Expression:"
      PP.<$>
      PP.indent 2 
          ( PP.pretty f PP.<$> PP.pretty xs )
          
    pretty (ExprLet lets expr) =
      PP.text "Let Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "let fields:" <+> PP.pretty lets
          PP.<$>
          PP.string "expression:" <+> PP.pretty expr
        )

    pretty (ExprIf pred thenBranch elseBranch) =
      PP.text "Let Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "predicate:" <+> PP.pretty pred
          PP.<$>
          PP.string "then branch:" <+> PP.pretty thenBranch
          PP.<$>
          PP.string "else branch:" <+> PP.pretty elseBranch
        )

    pretty (ExprMember expr mem) =
      PP.text "Member Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "expression:" <+> PP.pretty expr
          PP.<$>
          PP.string "member:" <+> PP.pretty mem
        )

    pretty (ExprIndex expr index)  =
      PP.text "Index Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "expression:" <+> PP.pretty expr
          PP.<$>
          PP.string "index:" <+> PP.pretty index
        )
        
    pretty (ExprTypeAnnot e t) =
      PP.text "Type Annotated Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "expression:" <+> PP.pretty e
          PP.<$>
          PP.string "type:" <+> PP.pretty t
        )

    pretty ExprBottom =
      PP.text "Bottom Expression"


instance (PP.Pretty n) => PP.Pretty (LetField n) where
    pretty (LetVar v) =
      PP.text "Let Variable:" <+> PP.pretty v

    pretty (LetFun f) =
      PP.text "Let Function:" <+> PP.pretty f

    pretty (LetSig s) =
      PP.text "Let Type Sig:" <+> PP.pretty s


-- Statement -------------------------------------------------------------------------
instance (PP.Pretty n) => PP.Pretty (Stmt n) where
    pretty (StmtExpr expr) =
      PP.text "Expression Statement:"
      PP.<$>
      PP.indent 2
        ( PP.string "expr:" <+> PP.pretty expr
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

    pretty (StmtIf pred thenStmt elseStmt) =
      PP.text "If Statement:"
      PP.<$>
      PP.indent 2
        ( PP.string "predicate:" <+> PP.pretty pred
          PP.<$>
          PP.string "then block:" <+> PP.pretty thenStmt
          PP.<$>
          PP.string "else block:" <+> PP.pretty elseStmt
        )

    pretty (StmtWhile cond stmtBlk) =
      PP.text "While Statement:"
      PP.<$>
      PP.indent 2
        ( PP.string "condition:" <+> PP.pretty cond
          PP.<$>
          PP.string "block:" <+> PP.pretty stmtBlk
        )
      
      
    pretty (StmtReturn expr) =
      PP.text "Return Statement:"
      PP.<$>
      PP.indent 2 ( PP.pretty expr )


-- Body -------------------------------------------------------------------------  
instance (PP.Pretty n) => PP.Pretty (Body n) where
    pretty (BodyBlock blk) =
      PP.text "Body Block:" <+> PP.pretty blk

    pretty (BodyExpr expr) =
      PP.text "Body Expression:" <+> PP.pretty expr


-- Type Signature ---------------------------------------------------------------
instance (PP.Pretty n) => PP.Pretty (TypeSig n) where
    pretty (TypeSig name body) =
      PP.text "Type Signature:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" <+> PP.pretty name
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- Variable ----------------------------------------------------------------------
instance (PP.Pretty n) => PP.Pretty (Var n) where
  pretty (Var name body) =
    PP.text "Variable Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )


-- Function ---------------------------------------------------------------------
instance (PP.Pretty n) => PP.Pretty (Fun n) where
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
instance (PP.Pretty n) => PP.Pretty (NewType n) where
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
instance (PP.Pretty n) => PP.Pretty (TypeAlias n) where
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
instance (PP.Pretty n) => PP.Pretty (TypeClass n) where
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
instance (PP.Pretty n) => PP.Pretty (TypeClassInst n) where
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
instance (PP.Pretty n) => PP.Pretty (DataType n) where
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
      _ -> undefined
      
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


instance Binary Home where
    put h =
      case h of
        Home fp r -> putWord8 0 >> put fp >> put r
        Builtin   -> putWord8 1
          
    get = do
      n <- getWord8
      case n of
        0 -> Home <$> get <*> get
        1 -> pure Builtin
        _ -> error "Corrupted data"


-- Type ------------------------------------------------------------------------
instance (Binary n) => Binary (Type n) where
  put tipe =
    case tipe of
      TypeFun t1 t2 -> putWord8 1 >> put t1 >> put t2
      TypeTuple ts  -> putWord8 2 >> put ts
      TypeApp tc ta -> putWord8 3 >> put tc >> put ta
      TypeVar n     -> putWord8 4 >> put n
      TypeCon n     -> putWord8 5 >> put n
        
  get =
    do  n <- getWord8
        case n of
          1 -> TypeFun <$> get <*> get
          2 -> TypeTuple <$> get
          3 -> TypeApp <$> get <*> get
          4 -> TypeVar <$> get
          5 -> TypeCon <$> get
          _ -> undefined


instance (Binary n) => Binary (QType n) where
  get =
    QType <$> get <*> get

  put (QType ctx tipe) =
    put ctx >> put tipe


-- Type Context --------------------------------------------------------------------

instance (Binary n) => Binary (TyContext n) where
  get =
    TyContext <$> get

  put (TyContext assrts) =
    put assrts

instance (Binary n) => Binary (TyAssert n) where
  get =
    TyAssert <$> get <*> get

  put (TyAssert con args) =
    put con >> put args

          
-- Expr -------------------------------------------------------------------------
instance (Binary n) => Binary (Expr n) where
  get = do
    n <- getWord8
    case n of
      1 -> ExprLit <$> get
      2 -> ExprVar <$> get
      3 -> ExprCon <$> get
      4 -> ExprAssign <$> get <*> get
      5 -> ExprLam <$> get <*> get
      6 -> ExprApp <$> get <*> get
      7 -> ExprLet <$> get <*> get
      8 -> ExprIf <$> get <*> get <*> get
      9 -> ExprMember <$> get <*> get
      10 -> ExprIndex <$> get <*> get
      11 -> ExprTypeAnnot <$> get <*> get
      12 -> pure ExprBottom
      _ -> undefined
      
  put e =
    case e of
      ExprLit l           -> putWord8 1 >> put l
      ExprVar n           -> putWord8 2 >> put n
      ExprCon n           -> putWord8 3 >> put n
      ExprAssign v rhs    -> putWord8 4 >> put v >> put rhs
      ExprLam p e         -> putWord8 5 >> put p >> put e
      ExprApp f a         -> putWord8 6 >> put f >> put a
      ExprLet lblk e      -> putWord8 7 >> put lblk >> put e
      ExprIf p a b        -> putWord8 8 >> put p >> put a >> put b
      ExprMember e n      -> putWord8 9 >> put e >> put n
      ExprIndex e i       -> putWord8 10 >> put e >> put i
      ExprTypeAnnot e t   -> putWord8 11 >> put e >> put t
      ExprBottom          -> putWord8 12

instance (Binary n) => Binary (LetField n) where
  get = do
    n <- getWord8
    case n of
      1 -> LetVar <$> get
      2 -> LetFun <$> get
      3 -> LetSig <$> get
      _ -> undefined

  put e =
    case e of
      LetVar v   -> putWord8 1 >> put v
      LetFun f   -> putWord8 2 >> put f
      LetSig s   -> putWord8 3 >> put s

-- Body ----------------------------------------------------------------------------
instance (Binary n) => Binary (Body n) where
  get = do
    n <- getWord8
    case n of
      1 -> BodyBlock <$> get
      2 -> BodyExpr <$> get
      _ -> undefined

  put e =
    case e of
      BodyBlock e   -> putWord8 1 >> put e
      BodyExpr b    -> putWord8 2 >> put b

-- Statement -------------------------------------------------------------------------
instance (Binary n) => Binary (Stmt n) where
  get = do
    n <- getWord8
    case n of
      1 -> StmtExpr <$> get
      2 -> StmtVar <$> get
      3 -> StmtFun <$> get
      4 -> StmtSig <$> get
      5 -> StmtIf <$> get <*> get <*> get
      6 -> StmtWhile <$> get <*> get
      7 -> StmtReturn <$> get
      _ -> undefined
      
  put e =
    case e of
      StmtExpr e                          -> putWord8 1 >> put e
      StmtVar v                           -> putWord8 2 >> put v
      StmtFun f                           -> putWord8 3 >> put f
      StmtSig s                           -> putWord8 4 >> put s
      StmtIf pred thenBlk elseBlk         -> putWord8 5 >> put pred >> put thenBlk >> put elseBlk
      StmtWhile cond blk                  -> putWord8 6 >> put cond >> put blk
      StmtReturn exp                      -> putWord8 7 >> put exp


-- Type Signature ---------------------------------------------------------------
instance (Binary n) => Binary (TypeSig n) where
  get =
    TypeSig <$> get <*> get

  put (TypeSig name body) =
    put name >> put body


-- Variable ----------------------------------------------------------------------
instance (Binary n) => Binary (Var n) where
  get =
      Var <$> get <*> get
      
  put (Var name body) =
      put name >> put body


-- Function ----------------------------------------------------------------------
instance (Binary n) => Binary (Fun n) where
  get =
      Fun <$> get <*> get <*> get
      
  put (Fun name params body) =
      put name >> put params >> put body


-- New Type ----------------------------------------------------------------------
instance (Binary n) => Binary (NewType n) where
  get =
    NewType <$> get <*> get <*> get

  put (NewType name tyvars body) =
    put name >> put tyvars >> put body


-- Type Alias ---------------------------------------------------------------------
instance (Binary n) => Binary (TypeAlias n) where
  get =
    TypeAlias <$> get <*> get <*> get

  put (TypeAlias name tyvars body) =
    put name >> put tyvars >> put body


-- Type Class ---------------------------------------------------------------------
instance (Binary n) => Binary (TypeClass n) where
  get =
    TypeClass <$> get <*> get <*> get <*> get

  put (TypeClass ctx name tyvars body) =
    put ctx >> put name >> put tyvars >> put body


-- Type Class Instance --------------------------------------------------------------
instance (Binary n) => Binary (TypeClassInst n) where
  get =
    TypeClassInst <$> get <*> get <*> get <*> get

  put (TypeClassInst ctx name args body) =
    put ctx >> put name >> put args >> put body


-- Data Type -----------------------------------------------------------------------
instance (Binary n) => Binary (DataType n) where
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
empty = Name "" Builtin


class HasBuiltin n where
  builtin :: Text -> n

instance HasBuiltin Name where
  builtin n =
    Name n Builtin

instance HasBuiltin QName where
  builtin n =
    QName n "" Builtin
      

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
    T.unpack n ++ " @ " ++ toString h
    
instance ToString Home where
    toString h =
      case h of
        Builtin ->
          "Builtin"
          
        Home fp (R.R (R.P r1 c1) (R.P r2 c2)) ->
          fp ++ ":" ++ show r1 ++ ":" ++ show c1 ++ "-" ++ show r2 ++ ":" ++ show c2


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

tyUnitConName :: HasBuiltin n => n
tyUnitConName = builtin "_#_Unit_#_"

tyFunConName :: HasBuiltin n => n
tyFunConName = builtin "_#_Fun_#_"

tyListConName :: HasBuiltin n => n
tyListConName = builtin "_#_List_#_"

tyTupleConName :: HasBuiltin n => Int -> n
tyTupleConName n = builtin $ T.pack ("_#_" ++ show n ++ "_Tuple_#_")