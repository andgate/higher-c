{-# LANGUAGE FlexibleInstances
           , OverloadedStrings
  #-}
module Language.Hawk.Syntax where

import Data.Binary
import Data.Text (Text)
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Operator
import Language.Hawk.Syntax.Prim
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
-- | Dependency

data Dependency =
  Dep
    { _depIsQual  :: Bool
    , _depPath    :: DepPath
    , _depAlias   :: Maybe Text
    } deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Dependency Path

data DepPath = 
    DepPath     Text DepPath
  | DepBase     Text
  | DepSpecify
    { _depSpecIsHidden  :: Bool
    , _depSpecfiers     :: [DepPath]
    }
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Nested Items

data NestedItem n
  = NestedVar (Var n)
  | NestedVal (Val n)
  | NestedFun (Fun n)
  | NestedVow (Vow n)
  | NestedSig (TypeSig n)
  deriving (Eq, Show, Ord)


-- -----------------------------------------------------------------------------
-- | Foreign

data Foreign n =
  Foreign ForeignType Text (TypeSig n)
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
-- | Name

data Home =
    Home
    { homePath :: FilePath
    , homeRegion :: R.Region
    }
  | Builtin
  deriving (Eq, Show, Ord)

data Name
  = Name 
    { _nameText :: Text
    , _nameHome :: Home
    , _nameIsQual :: Bool
    }
    deriving (Eq, Show, Ord)

data QName
  = QName
    { _qnamePath :: [Text]
    , _qnameBase :: Text
    , _qnameHome :: Home
    } deriving (Eq, Show, Ord)


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
  = ELit Literal
  | EVar n
  | ECon n

  | EApp (Expr n) [Expr n] -- Function application, which is left associative
  | EInfixApp (Expr n) -- rhs
              (Expr n) -- applied exp
              (Expr n) -- lhs

  | ELam [n] (Expr n)

  -- Control Flow
  | EDo [Stmt n]
  | EReturn (Expr n)
  
  | EIf (Expr n) -- conditional
        (Expr n) -- then body
        (Maybe (Expr n)) -- else body
  
  | EWhile (Expr n) -- Loop condition
           (Expr n) -- Loop body

-- Too complex for now 
--  | EFor (Expr n) -- Iterator
--         (Expr n) -- List
--         [Stmt n] -- Loop body

--  | ECase (Expr n)
--          (ECaseEntry n)


  -- Operators
  | EPrim PrimInstr (Expr n) (Expr n)
  | EBinary (Expr n) BinaryOp (Expr n)
  | EUnary UnaryOp (Expr n)
  | EAssign (Expr n) AssignOp (Expr n)

  -- Type hints and bottom
  | ETypeHint (Expr n) (QType n)
  | EBottom
  deriving (Eq, Show, Ord)

type SourceExpr = Expr Name

-- -----------------------------------------------------------------------------
-- | Statement

data Stmt n
  = StmtExpr (Expr n)
  | StmtDecl (NestedItem n)
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
    , _valBody  :: Body n
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

    pretty (ForeignItem i) =
      PP.pretty i

    pretty (ExposeItem i) =
      PP.pretty i

    pretty (VowItem i) =
      PP.pretty i

    pretty (SigItem i) =
      PP.pretty i

    pretty (VarItem i) =
      PP.pretty i

    pretty (ValItem i) =
      PP.pretty i

    pretty (FunItem i) =
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


-- Vested Item ---------------------------------------------------------------------
instance (PP.Pretty n) => PP.Pretty (NestedItem n) where
    pretty (NestedVar v) =
      PP.text "Nested Variable:" <+> PP.pretty v

    pretty (NestedVal v) =
      PP.text "Nested Value:" <+> PP.pretty v

    pretty (NestedFun f) =
      PP.text "Nested Function:" <+> PP.pretty f
    
    pretty (NestedVow v) =
      PP.text "Nested Vow:" <+> PP.pretty v

    pretty (NestedSig s) =
      PP.text "Nested Type Sig:" <+> PP.pretty s


-- Dependency ------------------------------------------------------------------
instance PP.Pretty Dependency where
    pretty (Dep ql p a) =
      PP.text "Dependency:"
      PP.<$>
      PP.indent 2
        ( PP.text "Is Qualified:" PP.<+> PP.pretty ql
          PP.<$>
          PP.text "Path:" PP.<+> PP.pretty p
          PP.<$>
          PP.text "Alias:" PP.<+> PP.text (show (T.unpack <$> a))
        )


-- Foreign ------------------------------------------------------------------
instance PP.Pretty n => PP.Pretty (Foreign n) where
    pretty (Foreign ft n fs) =
      PP.text "Foreign:"
      PP.<$>
      PP.indent 2
        ( PP.text "Foreign Type:" PP.<+> PP.pretty ft
          PP.<$>
          PP.text "Foreign Name:" PP.<+> PP.pretty (T.unpack n)
          PP.<$>
          PP.text "Foreign Sig:" PP.<+> PP.pretty fs
        )

instance PP.Pretty ForeignType where
    pretty ForeignC =
      PP.text "ForeignC"


-- Expose --------------------------------------------------------------------
instance PP.Pretty n => PP.Pretty (Expose n) where
    pretty (Expose n) =
      PP.text "Expose:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" PP.<+> PP.pretty n
        )


-- Dependency Path ------------------------------------------------------------
instance PP.Pretty DepPath where
    pretty (DepPath n r) =
      PP.text (T.unpack n) PP.<> PP.text "."  PP.<> PP.pretty r
        
    pretty (DepBase n) =
      PP.text (T.unpack n)
        
    pretty (DepSpecify False rs) =
      PP.text "(" PP.<> PP.pretty rs PP.<> PP.text ")"

    pretty (DepSpecify True rs) =
      PP.text "(\\" PP.<> PP.pretty rs PP.<> PP.text ")"


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
    pretty (ELit lit) =
      PP.text "Literal Expression:" PP.<+> PP.pretty lit

    pretty (EVar name) =
      PP.text "Variable Expression:" PP.<+> PP.pretty name
      
    pretty (ECon name) =
      PP.text "Constructor Expression:" PP.<+> PP.pretty name


    pretty (EApp f as) =
      PP.text "Application Expression:"
      PP.<$>
      PP.indent 2 
        ( PP.string "expression:" <+> PP.pretty f
          PP.<$>
          PP.string "applied to:" <+> PP.pretty as
        )

    pretty (EInfixApp l f r) =
      PP.text "Infix Application Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "expression:" <+> PP.pretty f
          PP.<$>
          PP.string "lhs:" <+> PP.pretty l
          PP.<$>
          PP.string "rhs:" <+> PP.pretty r
        )


    pretty (ELam ps b) =
      PP.text "Lambda Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "params:" <+> PP.pretty ps
          PP.<$>
          PP.string "body:" <+> PP.pretty b
        )

          
    pretty (EDo b) =
      PP.text "Do Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "body:" <+> PP.pretty b
        )

    pretty (EReturn e) =
      PP.text "Return Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "expression:" <+> PP.pretty e
        )

    pretty (EIf predicate thenBranch elseBranch) =
      PP.text "If Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "predicate:" <+> PP.pretty predicate
          PP.<$>
          PP.string "then branch:" <+> PP.pretty thenBranch
          PP.<$>
          PP.string "else branch:" <+> PP.pretty elseBranch
        )

    pretty (EWhile cond body) =
      PP.text "While Expression:"
      PP.<$>
      PP.indent 2
        ( PP.string "condition:" <+> PP.pretty cond
          PP.<$>
          PP.string "body:" <+> PP.pretty body
        )

    
    pretty (EPrim i a b) =
      PP.text "Primitive Instruction Expression:"
      PP.<$>
      PP.indent 2 
        ( PP.string "instruction:" <+> PP.pretty i
          PP.<$>
          PP.string "arg1:" <+> PP.pretty a
          PP.<$>
          PP.string "arg2:" <+> PP.pretty b
        )

    pretty (EBinary l o r) =
      PP.text "Binary Operator Expression:"
      PP.<$>
      PP.indent 2 
        ( PP.string "op:" <+> PP.pretty o
          PP.<$>
          PP.string "lhs:" <+> PP.pretty l
          PP.<$>
          PP.string "rhs:" <+> PP.pretty r
        )

    pretty (EUnary o e) =
      PP.text "Unary Operator Expression:"
      PP.<$>
      PP.indent 2 
        ( PP.string "op:" <+> PP.pretty o
          PP.<$>
          PP.string "expression:" <+> PP.pretty e
        )

    pretty (EAssign l o r) =
      PP.text "Assignment Expression:"
      PP.<$>
      PP.indent 2 
        ( PP.string "op:" <+> PP.pretty o
          PP.<$>
          PP.string "lhs:" <+> PP.pretty l
          PP.<$>
          PP.string "rhs:" <+> PP.pretty r
        )

        
    pretty (ETypeHint e t) =
      PP.text "Type Hint:"
      PP.<$>
      PP.indent 2
        ( PP.string "expression:" <+> PP.pretty e
          PP.<$>
          PP.string "hint:" <+> PP.pretty t
        )

    pretty EBottom =
      PP.text "Bottom Expression"


-- Statement -------------------------------------------------------------------------
instance (PP.Pretty n) => PP.Pretty (Stmt n) where
    pretty (StmtExpr expr) =
      PP.text "Statement Expression:"
      PP.<$>
      PP.indent 2 ( PP.pretty expr )
      
    pretty (StmtDecl i) =
      PP.text "Statement Declaration:"
      PP.<$>
      PP.indent 2 ( PP.pretty i )


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


-- Vow --------------------------------------------------------------------------
instance (PP.Pretty n) => PP.Pretty (Vow n) where
  pretty (Vow name vows) =
    PP.text "Vow Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "vows:" <+> PP.pretty vows
      )

instance PP.Pretty VowType where
  pretty VowVar =
    PP.text "Var"

  pretty VowVal =
    PP.text "Var"

  pretty VowRef =
    PP.text "Var"


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


-- Value -------------------------------------------------------------------------
instance (PP.Pretty n) => PP.Pretty (Val n) where
  pretty (Val name body) =
    PP.text "Value Item:"
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

-- Nested Item -----------------------------------------------------------------
instance (Binary n) => Binary (Item n) where
  get = do
    n <- getWord8
    case n of
      1  -> DepItem <$> get
      2  -> ForeignItem <$> get
      3  -> ExposeItem <$> get
      4  -> VowItem <$> get
      5  -> SigItem <$> get
      6  -> VarItem <$> get
      7  -> ValItem <$> get
      8  -> FunItem <$> get
      9  -> NewTyItem <$> get
      10 -> TyAliasItem <$> get
      11 -> TyClassItem <$> get
      12 -> TyInstItem <$> get
      13 -> DataItem <$> get
      _ -> undefined

  put e =
    case e of
      DepItem i       -> putWord8 1 >> put i
      ForeignItem i   -> putWord8 2 >> put i
      ExposeItem i    -> putWord8 3 >> put i
      VowItem i       -> putWord8 4 >> put i
      SigItem i       -> putWord8 5 >> put i
      VarItem i       -> putWord8 6 >> put i
      ValItem i       -> putWord8 7 >> put i
      FunItem i       -> putWord8 8 >> put i
      NewTyItem i     -> putWord8 9 >> put i
      TyAliasItem i   -> putWord8 10 >> put i
      TyClassItem i   -> putWord8 11 >> put i
      TyInstItem i    -> putWord8 12 >> put i
      DataItem i      -> putWord8 13 >> put i


-- Nested Item -----------------------------------------------------------------
instance (Binary n) => Binary (NestedItem n) where
  get = do
    n <- getWord8
    case n of
      1 -> NestedVar <$> get
      2 -> NestedVal <$> get
      3 -> NestedFun <$> get
      4 -> NestedVow <$> get
      5 -> NestedSig <$> get
      _ -> undefined

  put e =
    case e of
      NestedVar v   -> putWord8 1 >> put v
      NestedVal v   -> putWord8 2 >> put v
      NestedFun f   -> putWord8 3 >> put f
      NestedVow v   -> putWord8 4 >> put v
      NestedSig s   -> putWord8 5 >> put s


-- Dependency ---------------------------------------------------------------
instance Binary Dependency where
  get =
    Dep <$> get <*> get <*> get
      
  put (Dep qual path alias) =
    put qual >> put path >> put alias


-- Dependency Path -------------------------------------------------------------
instance Binary DepPath where
  get = do
    n <- getWord8
    case n of
      1 -> DepPath    <$> get <*> get
      2 -> DepBase    <$> get
      3 -> DepSpecify <$> get <*> get
      _ -> undefined
      
  put d =
    case d of
      DepPath n p       -> putWord8 1 >> put n >> put p
      DepBase n         -> putWord8 2 >> put n
      DepSpecify hq ns  -> putWord8 3 >> put hq >> put ns


-- Foreign ------------------------------------------------------------------
instance Binary n => Binary (Foreign n) where
  get =
    Foreign <$> get <*> get <*> get
      
  put (Foreign ft n fs) =
    put ft >> put n >> put fs


instance Binary ForeignType where
  get = do
    n <- getWord8
    case n of
      1 -> pure ForeignC
      _ -> undefined
      
  put f =
    case f of
      ForeignC  -> putWord8 1

-- Expose ---------------------------------------------------------------------
instance Binary n => Binary (Expose n) where
  get =
    Expose <$> get
      
  put (Expose n) =
    put n


-- Name ------------------------------------------------------------------------
instance Binary Name where
    put (Name h n q) =
      put h >> put n >> put q
          
    get =
      Name <$> get <*> get <*> get

instance Binary QName where
    put (QName path base home) =
      put path >> put base >> put home
          
    get =
      QName <$> get <*> get <*> get

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
      1 -> ELit <$> get
      2 -> EVar <$> get
      3 -> ECon <$> get

      4 -> EApp <$> get <*> get
      5 -> EInfixApp <$> get <*> get <*> get
      
      6 -> ELam <$> get <*> get
      
      7 -> EDo <$> get
      8 -> EReturn <$> get
      9 -> EIf <$> get <*> get <*> get
      10 -> EWhile <$> get <*> get

      11 -> EPrim <$> get <*> get <*> get
      12 -> EBinary <$> get <*> get <*> get
      13 -> EUnary <$> get <*> get
      14 -> EAssign <$> get <*> get <*> get
      
      15 -> ETypeHint <$> get <*> get
      16 -> pure EBottom
      _ -> undefined
      
  put e =
    case e of
      ELit l           -> putWord8 1 >> put l
      EVar n           -> putWord8 2 >> put n
      ECon n           -> putWord8 3 >> put n

      EApp f as        -> putWord8 4 >> put f >> put as
      EInfixApp l f r  -> putWord8 5 >> put l >> put f >> put r
      
      ELam p e         -> putWord8 6 >> put p >> put e

      EDo e            -> putWord8 7 >> put e 
      EReturn e        -> putWord8 8 >> put e
      EIf p a b        -> putWord8 9 >> put p >> put a >> put b
      EWhile c a       -> putWord8 10 >> put c >> put a

      EPrim i a b      -> putWord8 11 >> put i >> put a >> put b
      EBinary l o r    -> putWord8 12 >> put l >> put o >> put r
      EUnary o a       -> putWord8 13 >> put o >> put a
      EAssign l o r    -> putWord8 14 >> put l >> put o >> put r

      ETypeHint e t    -> putWord8 15 >> put e >> put t
      EBottom          -> putWord8 16

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
      2 -> StmtDecl <$> get
      _ -> undefined
      
  put e =
    case e of
      StmtExpr e     -> putWord8 1 >> put e
      StmtDecl i     -> putWord8 2 >> put i


-- Type Signature ---------------------------------------------------------------
instance (Binary n) => Binary (TypeSig n) where
  get =
    TypeSig <$> get <*> get

  put (TypeSig name body) =
    put name >> put body


-- Vow -------------------------------------------------------------------------
instance (Binary n) => Binary (Vow n) where
  get =
      Vow <$> get <*> get
      
  put (Vow name vs) =
      put name >> put vs


instance Binary VowType where
  get =
    do  n <- getWord8
        case n of
          1 -> pure VowVar
          2 -> pure VowVal
          3 -> pure VowRef
          _ -> undefined
      
  put v =
    case v of
      VowVar -> putWord8 1
      VowVal -> putWord8 2
      VowRef -> putWord8 3


-- Variable ----------------------------------------------------------------------
instance (Binary n) => Binary (Var n) where
  get =
      Var <$> get <*> get
      
  put (Var name body) =
      put name >> put body


-- Value -------------------------------------------------------------------------
instance (Binary n) => Binary (Val n) where
  get =
      Val <$> get <*> get
      
  put (Val name body) =
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
empty = Name "" Builtin False


splitQual :: Text -> [Text]
splitQual = T.splitOn "."


class HasBuiltin n where
  builtin :: Text -> n

instance HasBuiltin Name where
  builtin n =
    Name n Builtin False

instance HasBuiltin QName where
  builtin n =
    QName [] n Builtin


instance ToString Name where
  toString (Name n h q) =
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