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

import qualified Data.Text                        as T
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
  = TypeFun (Type n) (Type n)
  | TypeTuple [Type n]
  | TypeApp (Type n) [Type n]
  | TypeVar n
  | TypeCon n
  deriving (Eq, Show, Ord, D.Data, Typeable)


data QType n t
  = QType (TyContext n t) t
  deriving (Eq, Show, Ord, D.Data, Typeable)
  

newtype TyContext n t
  = TyContext [TyAssertion n t]
  deriving (Eq, Show, Ord, D.Data, Typeable)


data TyAssertion n t
  = TyAssert n [t]
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Expression

data Expr n t
  = ExprLit Literal
  | ExprVar n
  | ExprCon n

  | ExprAssign (Expr n t) (Body n (Expr n t) t)

  | ExprLam [n] (Expr n t)

  | ExprApp (Expr n t) [Expr n t]
  | ExprLet [LetField n t] (Expr n t)

  | ExprIf (Expr n t) (Expr n t) (Expr n t)

  -- Specific stucture access
  | ExprMember (Expr n t) n
  | ExprIndex (Expr n t) (Expr n t)

  | ExprTypeAnnot (Expr n t) t
  | ExprBottom
  deriving (Eq, Show, Ord, D.Data, Typeable)


data LetField n t
  = LetVar (Var n (Expr n t) t)
  | LetFun (Fun n (Expr n t) t)
  | LetSig (TypeSig (Expr n t) t)
  deriving (Eq, Show, Ord, D.Data, Typeable)



-- -----------------------------------------------------------------------------
-- | Statement

data Stmt n e t
  = StmtExpr e
  | StmtVar (Var n e t)
  | StmtFun (Fun n e t)
  | StmtSig (TypeSig n t)
  | StmtIf e [Stmt n e t] (Maybe [Stmt n e t])
  | StmtWhile e [Stmt n e t]
  | StmtReturn e
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Body

data Body n e t
  = BodyBlock [Stmt n e t]
  | BodyExpr (Expr n t)
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
    , _varBody  :: Maybe (Body n e t)
    }
  deriving (Eq, Show, Ord, D.Data, Typeable)


-- -----------------------------------------------------------------------------
-- | Function

data Fun n e t
  = Fun
    { _funName   :: n
    , _funParams :: [n]
    , _funBody   :: Body n e t
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
      { _tyClassContext :: Maybe (Context t)
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
      PP.text "Literal Expression:" PP.<$> PP.pretty lit
      
    pretty (ExprVar name) =
      PP.text "Variable Expression:" PP.<$> PP.pretty name
      
    pretty (ExprCon name) =
      PP.text "Constructor Expression:" PP.<$> PP.pretty name

    pretty (ExprAssign assignee rhs) =
      PP.text "Let Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "Assignee:" <+> PP.pretty assignee
          PP.<$>
          PP.string "rhs:" <+> PP.pretty rhs
        )

    pretty (ExprLam params expr) =
      PP.text "Lambda Expression:"
      PP.<$>
      PP.indent 2 
          ( PP.pretty params PP.<$> PP.pretty expr )

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


instance (PP.Pretty n, PP.Pretty t) => PP.Pretty (LetField n t ) where
    pretty (LetVar v) =
      PP.text "Let Variable:" <+> PP.pretty v

    pretty (LetFun f) =
      PP.text "Let Function:" <+> PP.pretty f

    pretty (LetSig s) =
      PP.text "Let Type Sig:" <+> PP.pretty s


-- Statement -------------------------------------------------------------------------
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Stmt n e t ) where
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
      PP.text "Return Expression:"
      PP.<$>
      PP.indent 2 ( PP.pretty expr )


-- Body -------------------------------------------------------------------------  
instance (PP.Pretty n, PP.Pretty e, PP.Pretty t) => PP.Pretty (Body n e t ) where
    pretty (BodyBlock blk) =
      PP.text "Body Block:" <+> PP.pretty blk

    pretty (BodyExpr expr) =
      PP.text "Body Expression:" <+> PP.pretty expr


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
  pretty (Var name body) =
    PP.text "Variable Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "body:" <+> PP.pretty body
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
      4 -> ExprAssign <$> get <*> get
      5 -> ExprLam <$> get <*> get
      6 -> ExprApp <$> get <*> get
      7 -> ExprLet <$> get <*> get
      8 -> ExprIf <$> get <*> get <*> get
      9 -> ExprMember <$> get <*> get
      10 -> ExprIndex <$> get <*> get
      11 -> ExprTypeAnnot <$> get <*> get
      12 -> pure ExprBottom
      
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

instance (Binary n, Binary t) => Binary (LetField n t) where
  get = do
    n <- getWord8
    case n of
      1 -> LetVar <$> get
      2 -> LetFun <$> get
      3 -> LetSig <$> get

  put e =
    case e of
      LetVar v   -> putWord8 1 >> put v
      LetFun f   -> putWord8 2 >> put f
      LetSig s   -> putWord8 3 >> put s

-- Body ----------------------------------------------------------------------------
instance (Binary n, Binary e, Binary t) => Binary (Body n e t) where
  get = do
    n <- getWord8
    case n of
      1 -> BodyBlock <$> get
      2 -> BodyExpr <$> get

  put e =
    case e of
      BodyBlock e   -> putWord8 1 >> put e
      BodyExpr b    -> putWord8 2 >> put b

-- Statement -------------------------------------------------------------------------
instance (Binary n, Binary e, Binary t) => Binary (Stmt n e t) where
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
      
  put e =
    case e of
      StmtExpr e                          -> putWord8 1 >> put e
      StmtVar v                           -> putWord8 2 >> put v
      StmtVar f                           -> putWord8 3 >> put f
      StmtVar s                           -> putWord8 4 >> put s
      StmtIf pred thenBlk elseBlk         -> putWord8 5 >> put pred >> put thenBlk >> put elseBlk
      StmtWhile cond blk                  -> putWord8 6 >> put cond >> put blk
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
    T.unpack n ++ " @ " ++ toString h
    
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

tyUnitConName :: HasBuiltin n => n
tyUnitConName = builtin "_#_Unit_#_"

tyFunConName :: HasBuiltin n => n
tyFunConName = builtin "_#_Fun_#_"

tyListConName :: HasBuiltin n => n
tyListConName = builtin "_#_List_#_"

tyTupleConName :: HasBuiltin n => Int -> n
tyTupleConName n = builtin $ T.pack ("_#_" ++ show n ++ "_Tuple_#_")


emptyCtx :: Context n
emptyCtx = Context []