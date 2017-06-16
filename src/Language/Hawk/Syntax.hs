{-# LANGUAGE TypeFamilies
           , GADTs
           , DataKinds
           , ConstraintKinds
           , EmptyCase
           , StandaloneDeriving
           , TypeOperators
           , PatternSynonyms
           , FlexibleInstances
           , FlexibleContexts
           , OverloadedStrings
           , UndecidableInstances
  #-}
module Language.Hawk.Syntax where

import Data.Binary
import Data.Text (Text)
import GHC.Types (Constraint)
import Language.Hawk.Syntax.HkLit
import Language.Hawk.Syntax.Operator
import Language.Hawk.Syntax.Pass
import Language.Hawk.Syntax.Prim
import Text.PrettyPrint.ANSI.Leijen ((<+>))

import qualified Data.Text                        as T
import qualified Language.Hawk.Report.Region      as R
import qualified Text.PrettyPrint.ANSI.Leijen     as PP


type ForallX (c :: * -> Constraint) (x :: *)
  = ( ForallHkLit c x
    , ForallHkExp c x
    , ForallHkType c x
    )

type ShowX (x :: *)
  = ForallX Show x

type EqX (x :: *)
  = ForallX Eq x

type OrdX (x :: *)
  = ForallX Ord x

type PrettyX (x :: *)
  = ForallX PP.Pretty x

type BinaryX (x :: *)
  = ForallX Binary x

-- -----------------------------------------------------------------------------
-- | Item

data Item x
  = DepItem Dependency

  | ForeignItem (Foreign x)
  | ExposeItem Expose

  | VowItem (Vow x)
  | SigItem (TypeSig x)
  | VarItem (Var x)
  | ValItem (Val x)
  | FunItem (Fun x)
  
  | NewTyItem (NewType x)
  | TyAliasItem (TypeAlias x)
  
  | TyClassItem (TypeClass x)
  | TyInstItem (TypeClassInst x)
  
  | DataItem (DataType x)

deriving instance (ShowX x, Show x) => Show (Item x)
deriving instance (EqX x, Eq x) => Eq (Item x)


-- -----------------------------------------------------------------------------
-- | Dependency

data Dependency =
  Dep
    { _depIsQual  :: Bool
    , _depPath    :: DepPath
    , _depAlias   :: Maybe Text
    } deriving (Eq, Show)


-- -----------------------------------------------------------------------------
-- | Dependency Path

data DepPath = 
    DepPath     Text DepPath
  | DepBase     Text
  | DepSpecify
    { _depSpecIsHidden  :: Bool
    , _depSpecfiers     :: [DepPath]
    }
  deriving (Eq, Show)


-- -----------------------------------------------------------------------------
-- | Nested Items

data NestedItem x
  = NestedVar (Var x)
  | NestedVal (Val x)
  | NestedFun (Fun x)
  | NestedVow (Vow x)
  | NestedSig (TypeSig x)

deriving instance ShowX x => Show (NestedItem x)
deriving instance EqX x => Eq (NestedItem x)


-- -----------------------------------------------------------------------------
-- | Foreign

data Foreign x =
  Foreign ForeignType Text (TypeSig x)

deriving instance (ShowType x, ShowExp x) => Show (Foreign x)
deriving instance (EqType x, EqExp x) => Eq (Foreign x)


data ForeignType =
  ForeignC
  deriving (Eq, Show)


-- -----------------------------------------------------------------------------
-- | Expose

newtype Expose =
  Expose Name
  deriving (Eq, Show)


-- -----------------------------------------------------------------------------
-- | Name

type Name = Text

-- Shouldn't need explicit name annotations
-- with eXtensible trees
{-
data Home =
    Home
    { homePath :: FilePath
    , homeRegion :: R.Region
    }
  | Builtin
  deriving (Eq, Show)

data Name
  = Name 
    { _nameText :: Text
    , _nameHome :: Home
    , _nameIsQual :: Bool
    }
    deriving (Eq, Show)

data QName
  = QName
    { _qnamePath :: [Text]
    , _qnameBase :: Text
    , _qnameHome :: Home
    } deriving (Eq, Show)
-}

-- -----------------------------------------------------------------------------
-- | Type

data HkType x
  = HkTyFun   (XHkTyFun x)    (HkType x) (HkType x)
  | HkTyTuple (XHkTyTuple x)  [HkType x]
  | HkTyApp   (XHkTyApp x)    (HkType x) [HkType x]
  | HkTyVar   (XHkTyVar x)    Name
  | HkTyCon   (XHkTyCon x)    Name
  | HkType    (XHkType x)

type family XHkTyFun x
type family XHkTyTuple x
type family XHkTyApp x
type family XHkTyVar x
type family XHkTyCon x
type family XHkType x


type ForallHkType (c :: * -> Constraint) (x :: *) =
  ( c (XHkTyFun x)
  , c (XHkTyTuple x)
  , c (XHkTyApp x)
  , c (XHkTyVar x)
  , c (XHkTyCon x)
  , c (XHkType x)
  )

type ShowType (x :: *)
  = ForallHkType Show x

type EqType (x :: *)
  = ForallHkType Eq x

type OrdType (x :: *)
  = ForallHkType Ord x

type PrettyType (x :: *)
  = ForallHkType PP.Pretty x

type BinaryType (x :: *)
  = ForallHkType Binary x

deriving instance ShowType x => Show (HkType x)
deriving instance EqType x => Eq (HkType x)

type instance XHkTyFun    HkcPs = ()
type instance XHkTyTuple  HkcPs = ()
type instance XHkTyApp    HkcPs = ()
type instance XHkTyVar    HkcPs = ()
type instance XHkTyCon    HkcPs = ()
type instance XHkType     HkcPs = ()

type instance XHkTyFun    HkcRn = ()
type instance XHkTyTuple  HkcRn = ()
type instance XHkTyApp    HkcRn = ()
type instance XHkTyVar    HkcRn = ()
type instance XHkTyCon    HkcRn = ()
type instance XHkType     HkcRn = ()

type instance XHkTyFun    HkcTc = ()
type instance XHkTyTuple  HkcTc = ()
type instance XHkTyApp    HkcTc = ()
type instance XHkTyVar    HkcTc = ()
type instance XHkTyCon    HkcTc = ()
type instance XHkType     HkcTc = ()


-- -----------------------------------------------------------------------------
-- | Qualified Type
data QType x
  = QType (TyContext x) (HkType x)

deriving instance (ForallHkType Eq x) => Eq (QType x)
deriving instance (ForallHkType Show x) => Show (QType x)


-- -----------------------------------------------------------------------------
-- | Type Context

data TyContext x
  = TyContext [TyAssert x]

deriving instance ForallHkType Show x => Show (TyContext x)
deriving instance ForallHkType Eq x => Eq (TyContext x)

data TyAssert x
  = TyAssert Name [HkType x]

deriving instance ForallHkType Show x => Show (TyAssert x)
deriving instance ForallHkType Eq x => Eq (TyAssert x)


-- -----------------------------------------------------------------------------
-- | Expression

data HkExp x
  = HkELit (XHkELit x) (HkLit x)
  | HkEVar (XHkEVar x) Name
  | HkECon (XHkECon x) Name

  | HkEApp (XHkEApp x) (HkExp x) [HkExp x] -- Function application, which is left associative
  | HkEInfixApp (XHkEInfixApp x) (HkExp x) -- rhs
                         (HkExp x) -- applied exp
                         (HkExp x) -- lhs

  | HkELam (XHkELam x) [Name] (HkExp x)

  -- Control Flow
  | HkEDo (XHkEDo x) [Stmt x]
  | HkEReturn (XHkEReturn x) (HkExp x)
  
  | HkEIf (XHkEIf x) (HkExp x) -- conditional
                   (HkExp x) -- then expr
                   (Maybe (HkExp x)) -- else exp
  
  | HkEWhile (XHkEWhile x) (HkExp x) -- Loop condition
                      (HkExp x) -- Loop body

-- Too complex for now 
--  | EFor (Exp ex) -- Iterator
--         (Exp ex) -- List
--         [Stmt n] -- Loop body

--  | ECase (Exp ex)
--          (ECaseEntry n)


  -- Operators
  | HkEPrim (XHkEPrim x) PrimInstr (HkExp x) (HkExp x)
  | HkEBinary (XHkEBinary x) (HkExp x) BinaryOp (HkExp x)
  | HkEUnary (XHkEUnary x) UnaryOp (HkExp x)
  | HkEAssign (XHkEAssign x) (HkExp x) AssignOp (HkExp x)

  -- Type hints and bottom
  | HkETypeHint (XHkETypeHint x) (HkExp x) (QType x)
  | HkEBottom (XHkEBottom x)
  | HkExp (XHkExp x)

type family XHkELit x
type family XHkEVar x
type family XHkECon x
type family XHkEApp x
type family XHkEInfixApp x
type family XHkELam x
type family XHkEDo x
type family XHkEReturn x
type family XHkEIf x
type family XHkEWhile x
type family XHkEPrim x
type family XHkEBinary x
type family XHkEUnary x
type family XHkEAssign x
type family XHkETypeHint x
type family XHkEBottom x
type family XHkExp x


type ForallHkExp (c :: * -> Constraint) (x :: *) =
  ( c (XHkELit x)
  , c (XHkEVar x)
  , c (XHkECon x)
  , c (XHkEApp x)
  , c (XHkEInfixApp x)
  , c (XHkELam x)
  , c (XHkEDo x)
  , c (XHkEReturn x)
  , c (XHkEPrim x)
  , c (XHkEIf x)
  , c (XHkEWhile x)
  , c (XHkEBinary x)
  , c (XHkEUnary x)
  , c (XHkEAssign x)
  , c (XHkETypeHint x)
  , c (XHkEBottom x)
  , c (XHkExp x)
  )

type ShowExp (x :: *)
  = ForallHkExp Show x

type EqExp (x :: *)
  = ForallHkExp Eq x

deriving instance ShowX x => Show (HkExp x)
deriving instance EqX x => Eq (HkExp x)

type instance XHkELit         HkcPs = ()
type instance XHkEVar         HkcPs = ()
type instance XHkECon         HkcPs = ()
type instance XHkEApp         HkcPs = ()
type instance XHkEInfixApp    HkcPs = ()
type instance XHkELam         HkcPs = ()
type instance XHkEDo          HkcPs = ()
type instance XHkEReturn      HkcPs = ()
type instance XHkEIf          HkcPs = ()
type instance XHkEWhile       HkcPs = ()
type instance XHkEPrim        HkcPs = ()
type instance XHkEBinary      HkcPs = ()
type instance XHkEUnary       HkcPs = ()
type instance XHkEAssign      HkcPs = ()
type instance XHkETypeHint    HkcPs = ()
type instance XHkEBottom      HkcPs = ()
type instance XHkEBottom      HkcPs = ()

type instance XHkELit         HkcRn = ()
type instance XHkEVar         HkcRn = ()
type instance XHkECon         HkcRn = ()
type instance XHkEApp         HkcRn = ()
type instance XHkEInfixApp    HkcRn = ()
type instance XHkELam         HkcRn = ()
type instance XHkEDo          HkcRn = ()
type instance XHkEReturn      HkcRn = ()
type instance XHkEIf          HkcRn = ()
type instance XHkEWhile       HkcRn = ()
type instance XHkEPrim        HkcRn = ()
type instance XHkEBinary      HkcRn = ()
type instance XHkEUnary       HkcRn = ()
type instance XHkEAssign      HkcRn = ()
type instance XHkETypeHint    HkcRn = ()
type instance XHkEBottom      HkcRn = ()
type instance XHkExp          HkcRn = ()

type instance XHkELit         HkcTc = ()
type instance XHkEVar         HkcTc = ()
type instance XHkECon         HkcTc = ()
type instance XHkEApp         HkcTc = ()
type instance XHkEInfixApp    HkcTc = ()
type instance XHkELam         HkcTc = ()
type instance XHkEDo          HkcTc = ()
type instance XHkEReturn      HkcTc = ()
type instance XHkEIf          HkcTc = ()
type instance XHkEWhile       HkcTc = ()
type instance XHkEPrim        HkcTc = ()
type instance XHkEBinary      HkcTc = ()
type instance XHkEUnary       HkcTc = ()
type instance XHkEAssign      HkcTc = ()
type instance XHkETypeHint    HkcTc = ()
type instance XHkEBottom      HkcTc = ()
type instance XHkExp          HkcRn = ()


-- -----------------------------------------------------------------------------
-- | Statement

data Stmt x
  = StmtExpr (HkExp x)
  | StmtDecl (NestedItem x)

deriving instance ShowX x => Show (Stmt x)
deriving instance EqX x => Eq (Stmt x)


-- -----------------------------------------------------------------------------
-- | Body

data Body x
  = BodyBlock [Stmt x]
  | BodyExpr (HkExp x)

deriving instance ShowX x => Show (Body x)
deriving instance EqX x => Eq (Body x)

-- -----------------------------------------------------------------------------
-- | Type Signature

data TypeSig x
  = TypeSig
    { _tySigName :: Name
    , _tySigBody :: QType x
    }

deriving instance ShowType x => Show (TypeSig x)
deriving instance EqType x => Eq (TypeSig x)


-- -----------------------------------------------------------------------------
-- | Type Signature

data Vow x
  = Vow
    { _vowName :: Name
    , _vows :: [VowType]
    }
  deriving (Eq, Show)

data VowType =
  VowVar
  | VowVal
  | VowRef
  deriving (Eq, Show)


-- -----------------------------------------------------------------------------
-- | Variable

data Var x
  = Var
    { _varName  :: Name
    , _varBody  :: Maybe (Body x)
    }

deriving instance ShowX x => Show (Var x)
deriving instance EqX x => Eq (Var x)


-- -----------------------------------------------------------------------------
-- | Variable

data Val x
  = Val
    { _valName  :: Name
    , _valBody  :: Body x
    }

deriving instance ShowX x => Show (Val x)
deriving instance EqX x => Eq (Val x)


-- -----------------------------------------------------------------------------
-- | Function

data Fun x
  = Fun
    { _funName   :: Name
    , _funParams :: [Name]
    , _funBody   :: Body x
    }

deriving instance ShowX x => Show (Fun x)
deriving instance EqX x => Eq (Fun x)


-- -----------------------------------------------------------------------------
-- | New Type

data NewType x
  = NewType
    { _newTyName     :: Name
    , _newTyVars     :: [Name]
    , _newTyNewBody  :: HkType x
    }

deriving instance ShowType x => Show (NewType x)
deriving instance EqType x => Eq (NewType x)

-- -----------------------------------------------------------------------------
-- | Type Alias

data TypeAlias x
    = TypeAlias
      { _tyAliasName   :: Name
      , _tyAliasTyVars :: [Name]
      , _tyAliasBody   :: HkType x
      }

deriving instance ShowType x => Show (TypeAlias x)
deriving instance EqType x => Eq (TypeAlias x)


-- -----------------------------------------------------------------------------
-- | Type Class

data TypeClass x
    = TypeClass 
      { _tyClassContext :: Maybe (TyContext x)
      , _tyClassName :: Name
      , _tyClassVars :: [Name]
      , _tyClassBody :: [Either (Fun x) (TypeSig x)]
      }

deriving instance ShowX x => Show (TypeClass x)
deriving instance EqX x => Eq (TypeClass x)


-- -----------------------------------------------------------------------------
-- | Type Class Instance

data TypeClassInst x
    = TypeClassInst 
      { _tyClassInstContext :: Maybe (TyContext x)
      , _tyClassInstName :: Name
      , _tyClassInstArgs :: [HkType x]
      , _tyClassInstBody :: [Fun x]
      }

deriving instance ShowX x => Show (TypeClassInst x)
deriving instance EqX x => Eq (TypeClassInst x)

-- -----------------------------------------------------------------------------
-- | Data Type

data DataType x
    = DataType
      { _dataTyName :: Name
      , _dataTyVars :: [Name]
      , _dataTyBody :: [TypeSig x]
      }

deriving instance ShowType x => Show (DataType x)
deriving instance EqType x => Eq (DataType x)


-- -----------------------------------------------------------------------------
-- | Pretty Printing Instances

instance (PP.Pretty l, PP.Pretty r) => PP.Pretty (Either l r) where
    pretty (Left l) =
      PP.pretty l

    pretty (Right r) =
      PP.pretty r 


-- Item ------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Item x) where
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
instance PrettyX x => PP.Pretty (NestedItem x) where
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
instance PrettyType x => PP.Pretty (Foreign x) where
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
instance PP.Pretty Expose where
    pretty (Expose n) =
      PP.text "Expose:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" PP.<+> PP.text (T.unpack n)
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
{-
instance PP.Pretty Name where
    pretty n =
      PP.string (toString n)
-}


-- Type ------------------------------------------------------------------------
instance ForallHkType PP.Pretty x => PP.Pretty (HkType x) where
    pretty (HkTyFun ext x y) =
      PP.text "Type Function:"
      PP.<$>
      PP.indent 2
        ( PP.text "from:" <+> PP.pretty x
          PP.<$>
          PP.text "to:" PP.<$> PP.pretty y
          PP.<$>
          PP.text "ext:" PP.<$> PP.pretty ext
        )

    pretty (HkTyTuple ext mems) =
      PP.text "Type Tuple:"
      PP.<$>
      PP.indent 2
        ( PP.text "members:" <+> PP.pretty mems
          PP.<$>
          PP.text "ext:" PP.<$> PP.pretty ext
        )
        

    pretty (HkTyApp ext con args) =
      PP.text "Type App:"
      PP.<$>
      PP.indent 2
        ( PP.text "con:" <+> PP.pretty con
          PP.<$>
          PP.text "args:" PP.<$> PP.pretty args
          PP.<$>
          PP.text "ext:" PP.<$> PP.pretty ext
        )
      
    pretty (HkTyVar ext name) =
      PP.text "Type Var" <+> PP.dquotes (PP.text $ T.unpack name)
      PP.<$>
      PP.text "ext:" PP.<$> PP.pretty ext

    pretty (HkTyCon ext name) =
      PP.text "Type Con" <+> PP.dquotes (PP.text $ T.unpack name)
      PP.<$>
      PP.text "ext:" PP.<$> PP.pretty ext

    pretty (HkType ext) =
      PP.text "Type Ext:"
      PP.<$>
      PP.indent 2
        ( PP.text "ext:" <+> PP.pretty ext
        )


instance PrettyType x => PP.Pretty (QType x) where
    pretty (QType ctx tipe) =
      PP.text "Qualified Type:"
      PP.<$>
      PP.indent 2
        ( PP.text "context:" <+> PP.pretty ctx
          PP.<$>
          PP.text "type:" <+> PP.pretty tipe
        )
        

instance PrettyType x => PP.Pretty (TyContext x) where
    pretty (TyContext assrts) =
      PP.text "Context:"
      PP.<$>
      PP.indent 2
        ( PP.text "Assertions:" <+> PP.pretty assrts
        )

instance PrettyType x => PP.Pretty (TyAssert x) where
    pretty (TyAssert con tys) =
      PP.text "Type Assertion:"
      PP.<$>
      PP.indent 2
        ( PP.text "Constructor:" <+> (PP.text $ T.unpack con)
          PP.<$>
          PP.text "Arguments:" <+> PP.pretty tys
        )


-- Expr -------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (HkExp x) where
    pretty (HkELit ext lit) =
      PP.text "Literal:" PP.<+> PP.pretty lit
      PP.<$>
      PP.string "ext:" <+> PP.pretty ext

    pretty (HkEVar ext name) =
      PP.text "Variable:" PP.<+> PP.pretty (PP.text $ T.unpack name)
      PP.<$>
      PP.string "ext:" <+> PP.pretty ext
      
    pretty (HkECon ext name) =
      PP.text "Constructor:" PP.<+> PP.pretty (PP.text $ T.unpack name)
      PP.<$>
      PP.string "ext:" <+> PP.pretty ext


    pretty (HkEApp ext f as) =
      PP.text "Application:"
      PP.<$>
      PP.indent 2 
        ( PP.string "expression:" <+> PP.pretty f
          PP.<$>
          PP.string "applied to:" <+> PP.pretty as
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (HkEInfixApp ext l f r) =
      PP.text "Infix Application:"
      PP.<$>
      PP.indent 2
        ( PP.string "expression:" <+> PP.pretty f
          PP.<$>
          PP.string "lhs:" <+> PP.pretty l
          PP.<$>
          PP.string "rhs:" <+> PP.pretty r
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )


    pretty (HkELam ext ps b) =
      PP.text "Lambda:"
      PP.<$>
      PP.indent 2
        ( PP.string "params:" <+> PP.pretty (T.unpack <$> ps)
          PP.<$>
          PP.string "body:" <+> PP.pretty b
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

          
    pretty (HkEDo ext b) =
      PP.text "Do:"
      PP.<$>
      PP.indent 2
        ( PP.string "body:" <+> PP.pretty b
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (HkEReturn ext e) =
      PP.text "Return:"
      PP.<$>
      PP.indent 2
        ( PP.string "expression:" <+> PP.pretty e
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (HkEIf ext predicate thenBranch elseBranch) =
      PP.text "If:"
      PP.<$>
      PP.indent 2
        ( PP.string "predicate:" <+> PP.pretty predicate
          PP.<$>
          PP.string "then branch:" <+> PP.pretty thenBranch
          PP.<$>
          PP.string "else branch:" <+> PP.pretty elseBranch
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (HkEWhile ext cond body) =
      PP.text "While:"
      PP.<$>
      PP.indent 2
        ( PP.string "condition:" <+> PP.pretty cond
          PP.<$>
          PP.string "body:" <+> PP.pretty body
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    
    pretty (HkEPrim ext i a b) =
      PP.text "Primitive Instruction:"
      PP.<$>
      PP.indent 2 
        ( PP.string "instruction:" <+> PP.pretty i
          PP.<$>
          PP.string "arg1:" <+> PP.pretty a
          PP.<$>
          PP.string "arg2:" <+> PP.pretty b
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (HkEBinary ext l o r) =
      PP.text "Binary Operator:"
      PP.<$>
      PP.indent 2 
        ( PP.string "op:" <+> PP.pretty o
          PP.<$>
          PP.string "lhs:" <+> PP.pretty l
          PP.<$>
          PP.string "rhs:" <+> PP.pretty r
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (HkEUnary ext o e) =
      PP.text "Unary Operator:"
      PP.<$>
      PP.indent 2 
        ( PP.string "op:" <+> PP.pretty o
          PP.<$>
          PP.string "expression:" <+> PP.pretty e
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (HkEAssign ext l o r) =
      PP.text "Assignment:"
      PP.<$>
      PP.indent 2 
        ( PP.string "op:" <+> PP.pretty o
          PP.<$>
          PP.string "lhs:" <+> PP.pretty l
          PP.<$>
          PP.string "rhs:" <+> PP.pretty r
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

        
    pretty (HkETypeHint ext e t) =
      PP.text "Type Hint:"
      PP.<$>
      PP.indent 2
        ( PP.string "expression:" <+> PP.pretty e
          PP.<$>
          PP.string "hint:" <+> PP.pretty t
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (HkEBottom ext) =
      PP.text "Bottom"
      PP.<$>
      PP.string "ext:" <+> PP.pretty ext

    pretty (HkExp ext) =
      PP.string "ext:" <+> PP.pretty ext


-- Statement -------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Stmt x) where
    pretty (StmtExpr expr) =
      PP.text "Statement Expression:"
      PP.<$>
      PP.indent 2 ( PP.pretty expr )
      
    pretty (StmtDecl i) =
      PP.text "Statement Declaration:"
      PP.<$>
      PP.indent 2 ( PP.pretty i )


-- Body -------------------------------------------------------------------------  
instance PrettyX x => PP.Pretty (Body x) where
    pretty (BodyBlock blk) =
      PP.text "Body Block:" <+> PP.pretty blk

    pretty (BodyExpr expr) =
      PP.text "Body Expression:" <+> PP.pretty expr


-- Type Signature ---------------------------------------------------------------
instance PrettyType x => PP.Pretty (TypeSig x) where
    pretty (TypeSig name body) =
      PP.text "Type Signature:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" <+> PP.text (T.unpack name)
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- Vow --------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Vow x) where
  pretty (Vow name vows) =
    PP.text "Vow Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.text (T.unpack name)
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
instance PrettyX x => PP.Pretty (Var x) where
  pretty (Var name body) =
    PP.text "Variable Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.text (T.unpack name)
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )


-- Value -------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Val x) where
  pretty (Val name body) =
    PP.text "Value Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.text (T.unpack name)
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )


-- Function ---------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Fun x) where
  pretty (Fun name params body) =
    PP.text "Function Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.text (T.unpack name)
        PP.<$>
        PP.text "params:" <+> PP.pretty (T.unpack <$> params)
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )


-- New Type ----------------------------------------------------------------------
instance PrettyType x => PP.Pretty (NewType x) where
    pretty (NewType name tyvars body) =
      PP.text "New Type:"
      PP.<$>
      PP.indent 2
        ( 
          PP.text "name:" <+> PP.text (T.unpack name)
          PP.<$>
          PP.text "tyvars:" <+> PP.pretty (T.unpack <$> tyvars)
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )

-- Type Alias ---------------------------------------------------------------------
instance PrettyType x => PP.Pretty (TypeAlias x) where
    pretty (TypeAlias name tyvars body) =
      PP.text "Type Alias:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" <+> PP.text (T.unpack name)
          PP.<$>
          PP.text "tyvars:" <+> PP.pretty (T.unpack <$> tyvars)
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- Type Class ---------------------------------------------------------------------
instance PrettyX x => PP.Pretty (TypeClass x) where
    pretty (TypeClass ctx name tyvars body) =
      PP.text "Type Class"
      PP.<$>
      PP.indent 2
        ( PP.text "context:" <+> PP.pretty ctx
          PP.<$>
          PP.text "name:" <+> PP.text (T.unpack name)
          PP.<$>
          PP.text "type vars:" <+> PP.pretty (T.unpack <$> tyvars)
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- Type Class Instance --------------------------------------------------------------
instance PrettyX x => PP.Pretty (TypeClassInst x) where
    pretty (TypeClassInst ctx name args body) =
      PP.text "Type Class Instance:"
      PP.<$>
      PP.indent 2
        ( PP.text "context:" <+> PP.pretty ctx
          PP.<$>
          PP.text "name:" <+> PP.text (T.unpack name)
          PP.<$>
          PP.text "args:" <+> PP.pretty args
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- Data Type -----------------------------------------------------------------------
instance PrettyType x => PP.Pretty (DataType x) where
    pretty (DataType name tyvars body) =
      PP.text "Data Type:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" <+> PP.text (T.unpack name)
          PP.<$>
          PP.text "tyvars:" <+> PP.pretty (T.unpack <$> tyvars)
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- -----------------------------------------------------------------------------
-- | Binary Instances

-- Nested Item -----------------------------------------------------------------
instance BinaryX x => Binary (Item x) where
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
instance BinaryX x => Binary (NestedItem x) where
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
instance BinaryX x => Binary (Foreign x) where
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
instance Binary Expose where
  get =
    Expose <$> get
      
  put (Expose n) =
    put n


-- Name ------------------------------------------------------------------------

-- To be replaced|subsumed by Trees that Grow annotations
{-
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

-}

-- Type ------------------------------------------------------------------------
instance (ForallHkType Binary x) => Binary (HkType x) where
  put tipe =
    case tipe of
      HkTyFun ex t1 t2      -> putWord8 1 >> put ex >> put t1 >> put t2
      HkTyTuple ex ts       -> putWord8 2 >> put ex >> put ts
      HkTyApp ex tc ta      -> putWord8 3 >> put ex >> put tc >> put ta
      HkTyVar ex n          -> putWord8 4 >> put ex >> put n
      HkTyCon ex n          -> putWord8 5 >> put ex >> put n
      HkType ex             -> putWord8 6 >> put ex
        
  get =
    do  n <- getWord8
        case n of
          1 -> HkTyFun <$> get <*> get <*> get
          2 -> HkTyTuple <$> get <*> get
          3 -> HkTyApp <$> get <*> get <*> get
          4 -> HkTyVar <$> get <*> get
          5 -> HkTyCon <$> get <*> get
          6 -> HkType <$> get
          _ -> undefined


instance BinaryType x => Binary (QType x) where
  get =
    QType <$> get <*> get

  put (QType ctx tipe) =
    put ctx >> put tipe


-- Type Context --------------------------------------------------------------------

instance BinaryType x => Binary (TyContext x) where
  get =
    TyContext <$> get

  put (TyContext assrts) =
    put assrts

instance BinaryType x => Binary (TyAssert x) where
  get =
    TyAssert <$> get <*> get

  put (TyAssert con args) =
    put con >> put args

          
-- Expr -------------------------------------------------------------------------
instance BinaryX x => Binary (HkExp x) where
  get = do
    n <- getWord8
    case n of
      1 -> HkELit <$> get <*> get
      2 -> HkEVar <$> get <*> get
      3 -> HkECon <$> get <*> get

      4 -> HkEApp <$> get <*> get <*> get
      5 -> HkEInfixApp <$> get <*> get <*> get <*> get
      
      6 -> HkELam <$> get <*> get <*> get
      
      7 -> HkEDo <$> get <*> get
      8 -> HkEReturn <$> get <*> get
      9 -> HkEIf <$> get <*> get <*> get <*> get
      10 -> HkEWhile <$> get <*> get <*> get

      11 -> HkEPrim <$> get <*> get <*> get <*> get
      12 -> HkEBinary <$> get <*> get <*> get <*> get
      13 -> HkEUnary <$> get <*> get <*> get
      14 -> HkEAssign <$> get <*> get <*> get <*> get
      
      15 -> HkETypeHint <$> get <*> get <*> get
      16 -> HkEBottom <$> get
      17 -> HkExp <$> get
      _ -> undefined
      
  put e =
    case e of
      HkELit ex l           -> putWord8 1 >> put ex >> put l
      HkEVar ex n           -> putWord8 2 >> put ex >> put n
      HkECon ex n           -> putWord8 3 >> put ex >> put n

      HkEApp ex f as        -> putWord8 4 >> put ex >> put f >> put as
      HkEInfixApp ex l f r  -> putWord8 5 >> put ex >> put l >> put f >> put r
      
      HkELam ex p e         -> putWord8 6 >> put ex >> put p >> put e

      HkEDo ex e            -> putWord8 7 >> put ex >> put e 
      HkEReturn ex e        -> putWord8 8 >> put ex >> put e
      HkEIf ex p a b        -> putWord8 9 >> put ex >> put p >> put a >> put b
      HkEWhile ex c a       -> putWord8 10 >> put ex >> put c >> put a

      HkEPrim ex i a b      -> putWord8 11 >> put ex >> put i >> put a >> put b
      HkEBinary ex l o r    -> putWord8 12 >> put ex >> put l >> put o >> put r
      HkEUnary ex o a       -> putWord8 13 >> put ex >> put o >> put a
      HkEAssign ex l o r    -> putWord8 14 >> put ex >> put l >> put o >> put r

      HkETypeHint ex e t    -> putWord8 15 >> put ex >> put e >> put t
      HkEBottom ex          -> putWord8 16 >> put ex
      HkExp ex              -> putWord8 17 >> put ex

-- Body ----------------------------------------------------------------------------
instance BinaryX x => Binary (Body x) where
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
instance BinaryX x => Binary (Stmt x) where
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
instance BinaryX x => Binary (TypeSig x) where
  get =
    TypeSig <$> get <*> get

  put (TypeSig name body) =
    put name >> put body


-- Vow -------------------------------------------------------------------------
instance BinaryX x => Binary (Vow x) where
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
instance BinaryX x => Binary (Var x) where
  get =
      Var <$> get <*> get
      
  put (Var name body) =
      put name >> put body


-- Value -------------------------------------------------------------------------
instance BinaryX x => Binary (Val x) where
  get =
      Val <$> get <*> get
      
  put (Val name body) =
      put name >> put body


-- Function ----------------------------------------------------------------------
instance BinaryX x => Binary (Fun x) where
  get =
      Fun <$> get <*> get <*> get
      
  put (Fun name params body) =
      put name >> put params >> put body


-- New Type ----------------------------------------------------------------------
instance BinaryType x => Binary (NewType x) where
  get =
    NewType <$> get <*> get <*> get

  put (NewType name tyvars body) =
    put name >> put tyvars >> put body


-- Type Alias ---------------------------------------------------------------------
instance BinaryType x => Binary (TypeAlias x) where
  get =
    TypeAlias <$> get <*> get <*> get

  put (TypeAlias name tyvars body) =
    put name >> put tyvars >> put body


-- Type Class ---------------------------------------------------------------------
instance BinaryX x => Binary (TypeClass x) where
  get =
    TypeClass <$> get <*> get <*> get <*> get

  put (TypeClass ctx name tyvars body) =
    put ctx >> put name >> put tyvars >> put body


-- Type Class Instance --------------------------------------------------------------
instance BinaryX x => Binary (TypeClassInst x) where
  get =
    TypeClassInst <$> get <*> get <*> get <*> get

  put (TypeClassInst ctx name args body) =
    put ctx >> put name >> put args >> put body


-- Data Type -----------------------------------------------------------------------
instance BinaryX x => Binary (DataType x) where
  get =
    DataType <$> get <*> get <*> get

  put (DataType name tyvars body) =
    put name >> put tyvars >> put body


-- -----------------------------------------------------------------------------
-- | Helpers

-- Possibly useless
{-
class ToString a where
  toString :: a -> String
-}

-- Literal ---------------------------------------------------------------------
-- Is this actually used? Why not show?
{-
instance ToString Literal where 
  toString literal =
    case literal of
      IntNum n -> show n
      FloatNum n -> show n
      Chr c -> show c
      Str s -> s
      Boolean bool -> show bool
-}  

-- Name ------------------------------------------------------------------------

-- Currently being refactored in light of "Trees That Grow"
{-
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


-}

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

-- Why not just use the symbolic constructors?
{-
tyUnitConName :: HasBuiltin n => n
tyUnitConName = builtin "_#_Unit_#_"

tyFunConName :: HasBuiltin n => n
tyFunConName = builtin "_#_Fun_#_"

tyListConName :: HasBuiltin n => n
tyListConName = builtin "_#_List_#_"

tyTupleConName :: HasBuiltin n => Int -> n
tyTupleConName n = builtin $ T.pack ("_#_" ++ show n ++ "_Tuple_#_")
-}