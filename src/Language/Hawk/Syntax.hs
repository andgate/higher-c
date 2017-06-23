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
           , TemplateHaskell
  #-}
module Language.Hawk.Syntax where

import Data.Binary
import Data.Text (Text)
import Control.Lens
import GHC.Types (Constraint)
import Language.Hawk.Report.SrcLoc
import Language.Hawk.Syntax.Operator
import Language.Hawk.Syntax.Pass
import Language.Hawk.Syntax.Prim
import Text.PrettyPrint.ANSI.Leijen ((<+>))

import qualified Data.Text                        as T
import qualified Text.PrettyPrint.ANSI.Leijen     as PP


type ForallX (c :: * -> Constraint) (x :: *)
  = ( ForallLit c x
    , ForallExp c x
    , ForallType c x
    , c (XName x)
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
  | ExposeItem (Expose x)

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

deriving instance ShowX x => Show (Item x)
deriving instance EqX x => Eq (Item x)

type ItemPs = Item HkcPs


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

deriving instance ShowX x => Show (Foreign x)
deriving instance EqX x => Eq (Foreign x)


data ForeignType =
  ForeignC
  deriving (Eq, Show)


-- -----------------------------------------------------------------------------
-- | Expose

data Expose x =
  Expose (Name x)


deriving instance ShowX x => Show (Expose x)
deriving instance EqX x => Eq (Expose x)


-- -----------------------------------------------------------------------------
-- | Name

data Name x
  = Name 
    { _nameText :: Text
    , _nameExt  :: XName x
    }

type family XName x

type instance XName HkcPs = SrcLoc
type instance XName HkcRn = ()
type instance XName HkcTc = ()

deriving instance ShowX x => Show (Name x)
deriving instance EqX x => Eq (Name x)

type NamePs = Name HkcPs


-- -----------------------------------------------------------------------------
-- | Literal

data Lit x
  = IntLit (XIntLit x) Integer
  | DblLit (XDblLit x) Double
  | ChrLit (XChrLit x) Char
  | StrLit (XStrLit x) String
  | BoolLit (XBoolLit x) Bool
  | Lit (XLit x)

type family XIntLit x
type family XDblLit x
type family XChrLit x
type family XStrLit x
type family XBoolLit x
type family XLit x

type ForallLit (c :: * -> Constraint) (x :: *) =
  ( c (XIntLit x)
  , c (XDblLit x)
  , c (XChrLit x)
  , c (XStrLit x)
  , c (XBoolLit x)
  , c (XLit x)
  )


deriving instance ShowX x => Show (Lit x)
deriving instance EqX x => Eq (Lit x)


type instance XIntLit      HkcPs = ()
type instance XDblLit    HkcPs = ()
type instance XChrLit     HkcPs = ()
type instance XStrLit   HkcPs = ()
type instance XBoolLit  HkcPs = ()
type instance XLit      HkcPs = ()


type instance XIntLit      HkcRn = ()
type instance XDblLit    HkcRn = ()
type instance XChrLit     HkcRn = ()
type instance XStrLit   HkcRn = ()
type instance XBoolLit  HkcRn = ()
type instance XLit      HkcRn = ()


type instance XIntLit      HkcTc = ()
type instance XDblLit    HkcTc = ()
type instance XChrLit     HkcTc = ()
type instance XStrLit   HkcTc = ()
type instance XBoolLit  HkcTc = ()
type instance XLit      HkcTc = ()


-- -----------------------------------------------------------------------------
-- | Type

data Type x
  = TyFun   (XTyFun x)    (Type x) (Type x)
  | TyTuple (XTyTuple x)  [Type x]
  | TyApp   (XTyApp x)    (Type x) [Type x]
  | TyVar   (XTyVar x)    (Name x)
  | TyCon   (XTyCon x)    (Name x)
  | Type    (XType x)

type family XTyFun x
type family XTyTuple x
type family XTyApp x
type family XTyVar x
type family XTyCon x
type family XType x


type ForallType (c :: * -> Constraint) (x :: *) =
  ( c (XTyFun x)
  , c (XTyTuple x)
  , c (XTyApp x)
  , c (XTyVar x)
  , c (XTyCon x)
  , c (XType x)
  )

deriving instance ShowX x => Show (Type x)
deriving instance EqX x => Eq (Type x)

type instance XTyFun    HkcPs = ()
type instance XTyTuple  HkcPs = ()
type instance XTyApp    HkcPs = ()
type instance XTyVar    HkcPs = ()
type instance XTyCon    HkcPs = ()
type instance XType     HkcPs = ()

type instance XTyFun    HkcRn = ()
type instance XTyTuple  HkcRn = ()
type instance XTyApp    HkcRn = ()
type instance XTyVar    HkcRn = ()
type instance XTyCon    HkcRn = ()
type instance XType     HkcRn = ()

type instance XTyFun    HkcTc = ()
type instance XTyTuple  HkcTc = ()
type instance XTyApp    HkcTc = ()
type instance XTyVar    HkcTc = ()
type instance XTyCon    HkcTc = ()
type instance XType     HkcTc = ()


type TypePs = Type HkcPs

-- -----------------------------------------------------------------------------
-- | Qualified Type
data QType x
  = QType (TyContext x) (Type x)

deriving instance ShowX x => Show (QType x)
deriving instance EqX x => Eq (QType x)


-- -----------------------------------------------------------------------------
-- | Type Context

data TyContext x
  = TyContext [TyAssert x]

deriving instance ShowX x => Show (TyContext x)
deriving instance EqX x => Eq (TyContext x)

data TyAssert x
  = TyAssert (Name x) [Type x]

deriving instance ShowX x => Show (TyAssert x)
deriving instance EqX x => Eq (TyAssert x)


-- -----------------------------------------------------------------------------
-- | Expression

data Exp x
  = ELit (XELit x) (Lit x)
  | EVar (XEVar x) (Name x)
  | ECon (XECon x) (Name x)

  | EApp (XEApp x) (Exp x) [Exp x] -- Function application, which is left associative
  | EInfixApp (XEInfixApp x) (Exp x) -- rhs
                         (Exp x) -- applied exp
                         (Exp x) -- lhs

  | ELam (XELam x) [Name x] (Exp x)

  -- Control Flow
  | EDo (XEDo x) [Stmt x]
  | EReturn (XEReturn x) (Exp x)
  
  | EIf (XEIf x) (Exp x) -- conditional
                   (Exp x) -- then expr
                   (Maybe (Exp x)) -- else exp
  
  | EWhile (XEWhile x) (Exp x) -- Loop condition
                      (Exp x) -- Loop body

-- Too complex for now 
--  | EFor (Exp ex) -- Iterator
--         (Exp ex) -- List
--         [Stmt n] -- Loop body

--  | ECase (Exp ex)
--          (ECaseEntry n)


  -- Operators
  | EPrim (XEPrim x) PrimInstr (Exp x) (Exp x)
  | EBinary (XEBinary x) (Exp x) BinaryOp (Exp x)
  | EUnary (XEUnary x) UnaryOp (Exp x)
  | EAssign (XEAssign x) (Exp x) AssignOp (Exp x)

  -- Type hints and bottom
  | ETypeHint (XETypeHint x) (Exp x) (QType x)
  | EBottom (XEBottom x)
  | Exp (XExp x)

type family XELit x
type family XEVar x
type family XECon x
type family XEApp x
type family XEInfixApp x
type family XELam x
type family XEDo x
type family XEReturn x
type family XEIf x
type family XEWhile x
type family XEPrim x
type family XEBinary x
type family XEUnary x
type family XEAssign x
type family XETypeHint x
type family XEBottom x
type family XExp x


type ForallExp (c :: * -> Constraint) (x :: *) =
  ( c (XELit x)
  , c (XEVar x)
  , c (XECon x)
  , c (XEApp x)
  , c (XEInfixApp x)
  , c (XELam x)
  , c (XEDo x)
  , c (XEReturn x)
  , c (XEPrim x)
  , c (XEIf x)
  , c (XEWhile x)
  , c (XEBinary x)
  , c (XEUnary x)
  , c (XEAssign x)
  , c (XETypeHint x)
  , c (XEBottom x)
  , c (XExp x)
  )

deriving instance ShowX x => Show (Exp x)
deriving instance EqX x => Eq (Exp x)

type instance XELit         HkcPs = ()
type instance XEVar         HkcPs = ()
type instance XECon         HkcPs = ()
type instance XEApp         HkcPs = ()
type instance XEInfixApp    HkcPs = ()
type instance XELam         HkcPs = ()
type instance XEDo          HkcPs = ()
type instance XEReturn      HkcPs = ()
type instance XEIf          HkcPs = ()
type instance XEWhile       HkcPs = ()
type instance XEPrim        HkcPs = ()
type instance XEBinary      HkcPs = ()
type instance XEUnary       HkcPs = ()
type instance XEAssign      HkcPs = ()
type instance XETypeHint    HkcPs = ()
type instance XEBottom      HkcPs = ()
type instance XExp          HkcPs = ()

type instance XELit         HkcRn = ()
type instance XEVar         HkcRn = ()
type instance XECon         HkcRn = ()
type instance XEApp         HkcRn = ()
type instance XEInfixApp    HkcRn = ()
type instance XELam         HkcRn = ()
type instance XEDo          HkcRn = ()
type instance XEReturn      HkcRn = ()
type instance XEIf          HkcRn = ()
type instance XEWhile       HkcRn = ()
type instance XEPrim        HkcRn = ()
type instance XEBinary      HkcRn = ()
type instance XEUnary       HkcRn = ()
type instance XEAssign      HkcRn = ()
type instance XETypeHint    HkcRn = ()
type instance XEBottom      HkcRn = ()
type instance XExp          HkcRn = ()

type instance XELit         HkcTc = ()
type instance XEVar         HkcTc = ()
type instance XECon         HkcTc = ()
type instance XEApp         HkcTc = ()
type instance XEInfixApp    HkcTc = ()
type instance XELam         HkcTc = ()
type instance XEDo          HkcTc = ()
type instance XEReturn      HkcTc = ()
type instance XEIf          HkcTc = ()
type instance XEWhile       HkcTc = ()
type instance XEPrim        HkcTc = ()
type instance XEBinary      HkcTc = ()
type instance XEUnary       HkcTc = ()
type instance XEAssign      HkcTc = ()
type instance XETypeHint    HkcTc = ()
type instance XEBottom      HkcTc = ()
type instance XExp          HkcRn = ()


type ExpPs = Exp HkcPs

-- -----------------------------------------------------------------------------
-- | Statement

data Stmt x
  = StmtExpr (Exp x)
  | StmtDecl (NestedItem x)

deriving instance ShowX x => Show (Stmt x)
deriving instance EqX x => Eq (Stmt x)


-- -----------------------------------------------------------------------------
-- | Body

data Body x
  = BodyBlock [Stmt x]
  | BodyExpr (Exp x)

deriving instance ShowX x => Show (Body x)
deriving instance EqX x => Eq (Body x)

-- -----------------------------------------------------------------------------
-- | Type Signature

data TypeSig x
  = TypeSig
    { _tySigName :: Name x
    , _tySigBody :: QType x
    }

deriving instance ShowX x => Show (TypeSig x)
deriving instance EqX x => Eq (TypeSig x)


-- -----------------------------------------------------------------------------
-- | Type Signature

data Vow x
  = Vow
    { _vowName :: Name x
    , _vows :: [VowType]
    }

deriving instance ShowX x => Show (Vow x)
deriving instance EqX x => Eq (Vow x)

data VowType =
  VowVar
  | VowVal
  | VowRef
  deriving (Eq, Show)


-- -----------------------------------------------------------------------------
-- | Variable

data Var x
  = Var
    { _varName  :: Name x
    , _varBody  :: Maybe (Body x)
    }

deriving instance ShowX x => Show (Var x)
deriving instance EqX x => Eq (Var x)


-- -----------------------------------------------------------------------------
-- | Variable

data Val x
  = Val
    { _valName  :: Name x
    , _valBody  :: Body x
    }

deriving instance ShowX x => Show (Val x)
deriving instance EqX x => Eq (Val x)


-- -----------------------------------------------------------------------------
-- | Function

data Fun x
  = Fun
    { _funName   :: Name x
    , _funParams :: [Name x]
    , _funBody   :: Body x
    }

deriving instance ShowX x => Show (Fun x)
deriving instance EqX x => Eq (Fun x)

-- -----------------------------------------------------------------------------
-- | New Type

data NewType x
  = NewType
    { _newTyName     :: Name x
    , _newTyVars     :: [Name x]
    , _newTyNewBody  :: Type x
    }

deriving instance ShowX x => Show (NewType x)
deriving instance EqX x => Eq (NewType x)

-- -----------------------------------------------------------------------------
-- | Type Alias

data TypeAlias x
    = TypeAlias
      { _tyAliasName   :: Name x
      , _tyAliasTyVars :: [Name x]
      , _tyAliasBody   :: Type x
      }

deriving instance ShowX x => Show (TypeAlias x)
deriving instance EqX x => Eq (TypeAlias x)


-- -----------------------------------------------------------------------------
-- | Type Class

data TypeClass x
    = TypeClass 
      { _tyClassContext :: Maybe (TyContext x)
      , _tyClassName :: Name x
      , _tyClassVars :: [Name x]
      , _tyClassBody :: [Either (Fun x) (TypeSig x)]
      }

deriving instance ShowX x => Show (TypeClass x)
deriving instance EqX x => Eq (TypeClass x)


-- -----------------------------------------------------------------------------
-- | Type Class Instance

data TypeClassInst x
    = TypeClassInst 
      { _tyClassInstContext :: Maybe (TyContext x)
      , _tyClassInstName :: Name x
      , _tyClassInstArgs :: [Type x]
      , _tyClassInstBody :: [Fun x]
      }

deriving instance ShowX x => Show (TypeClassInst x)
deriving instance EqX x => Eq (TypeClassInst x)

-- -----------------------------------------------------------------------------
-- | Data Type

data DataType x
    = DataType
      { _dataTyName :: Name x
      , _dataTyVars :: [Name x]
      , _dataTyBody :: [TypeSig x]
      }

deriving instance ShowX x => Show (DataType x)
deriving instance EqX x => Eq (DataType x)


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
instance PrettyX x => PP.Pretty (Foreign x) where
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
instance PrettyX x => PP.Pretty (Expose x) where
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

instance PrettyX x => PP.Pretty (Name x) where
    pretty (Name n ex) =
      PP.text (T.unpack n)
      PP.<$>
      PP.pretty ex


-- Literal ------------------------------------------------------------------------

instance PrettyX x => PP.Pretty (Lit x) where
  pretty lit =
    case lit of
      IntLit x v ->
        PP.string (show v)
        PP.<$>
        PP.string "Ext:" <+> PP.pretty x
         
      DblLit x v ->
        PP.string (show v)
        PP.<$>
        PP.string "Ext:" <+> PP.pretty x
      
      ChrLit x v ->
        PP.string (show v)
        PP.<$>
        PP.string "Ext:" <+> PP.pretty x
      
      StrLit x v ->
        PP.string (show v)
        PP.<$>
        PP.string "Ext:" <+> PP.pretty x
      
      BoolLit x v ->
        PP.string (show v)
        PP.<$>
        PP.string "Ext:" <+> PP.pretty x

      Lit x ->
        PP.string "Lit Con Ext:" <+> PP.pretty x


-- Type ------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Type x) where
    pretty (TyFun ext x y) =
      PP.text "Type Function:"
      PP.<$>
      PP.indent 2
        ( PP.text "from:" <+> PP.pretty x
          PP.<$>
          PP.text "to:" PP.<$> PP.pretty y
          PP.<$>
          PP.text "ext:" PP.<$> PP.pretty ext
        )

    pretty (TyTuple ext mems) =
      PP.text "Type Tuple:"
      PP.<$>
      PP.indent 2
        ( PP.text "members:" <+> PP.pretty mems
          PP.<$>
          PP.text "ext:" PP.<$> PP.pretty ext
        )
        

    pretty (TyApp ext con args) =
      PP.text "Type App:"
      PP.<$>
      PP.indent 2
        ( PP.text "con:" <+> PP.pretty con
          PP.<$>
          PP.text "args:" PP.<$> PP.pretty args
          PP.<$>
          PP.text "ext:" PP.<$> PP.pretty ext
        )
      
    pretty (TyVar ext name) =
      PP.text "Type Var" <+> PP.dquotes (PP.pretty name)
      PP.<$>
      PP.text "ext:" PP.<$> PP.pretty ext

    pretty (TyCon ext name) =
      PP.text "Type Con" <+> PP.dquotes (PP.pretty name)
      PP.<$>
      PP.text "ext:" PP.<$> PP.pretty ext

    pretty (Type ext) =
      PP.text "Type Ext:"
      PP.<$>
      PP.indent 2
        ( PP.text "ext:" <+> PP.pretty ext
        )


instance PrettyX x => PP.Pretty (QType x) where
    pretty (QType ctx tipe) =
      PP.text "Qualified Type:"
      PP.<$>
      PP.indent 2
        ( PP.text "context:" <+> PP.pretty ctx
          PP.<$>
          PP.text "type:" <+> PP.pretty tipe
        )
        

instance PrettyX x => PP.Pretty (TyContext x) where
    pretty (TyContext assrts) =
      PP.text "Context:"
      PP.<$>
      PP.indent 2
        ( PP.text "Assertions:" <+> PP.pretty assrts
        )

instance PrettyX x => PP.Pretty (TyAssert x) where
    pretty (TyAssert con tys) =
      PP.text "Type Assertion:"
      PP.<$>
      PP.indent 2
        ( PP.text "Constructor:" <+> PP.pretty con
          PP.<$>
          PP.text "Arguments:" <+> PP.pretty tys
        )


-- Expr -------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Exp x) where
    pretty (ELit ext lit) =
      PP.text "Literal:" PP.<+> PP.pretty lit
      PP.<$>
      PP.string "ext:" <+> PP.pretty ext

    pretty (EVar ext name) =
      PP.text "Variable:" PP.<+> PP.pretty name
      PP.<$>
      PP.string "ext:" <+> PP.pretty ext
      
    pretty (ECon ext name) =
      PP.text "Constructor:" PP.<+> PP.pretty name
      PP.<$>
      PP.string "ext:" <+> PP.pretty ext


    pretty (EApp ext f as) =
      PP.text "Application:"
      PP.<$>
      PP.indent 2 
        ( PP.string "expression:" <+> PP.pretty f
          PP.<$>
          PP.string "applied to:" <+> PP.pretty as
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (EInfixApp ext l f r) =
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


    pretty (ELam ext ps b) =
      PP.text "Lambda:"
      PP.<$>
      PP.indent 2
        ( PP.string "params:" <+> PP.pretty ps
          PP.<$>
          PP.string "body:" <+> PP.pretty b
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

          
    pretty (EDo ext b) =
      PP.text "Do:"
      PP.<$>
      PP.indent 2
        ( PP.string "body:" <+> PP.pretty b
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (EReturn ext e) =
      PP.text "Return:"
      PP.<$>
      PP.indent 2
        ( PP.string "expression:" <+> PP.pretty e
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (EIf ext predicate thenBranch elseBranch) =
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

    pretty (EWhile ext cond body) =
      PP.text "While:"
      PP.<$>
      PP.indent 2
        ( PP.string "condition:" <+> PP.pretty cond
          PP.<$>
          PP.string "body:" <+> PP.pretty body
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    
    pretty (EPrim ext i a b) =
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

    pretty (EBinary ext l o r) =
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

    pretty (EUnary ext o e) =
      PP.text "Unary Operator:"
      PP.<$>
      PP.indent 2 
        ( PP.string "op:" <+> PP.pretty o
          PP.<$>
          PP.string "expression:" <+> PP.pretty e
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (EAssign ext l o r) =
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

        
    pretty (ETypeHint ext e t) =
      PP.text "Type Hint:"
      PP.<$>
      PP.indent 2
        ( PP.string "expression:" <+> PP.pretty e
          PP.<$>
          PP.string "hint:" <+> PP.pretty t
          PP.<$>
          PP.string "ext:" <+> PP.pretty ext
        )

    pretty (EBottom ext) =
      PP.text "Bottom"
      PP.<$>
      PP.string "ext:" <+> PP.pretty ext

    pretty (Exp ext) =
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
instance PrettyX x => PP.Pretty (TypeSig x) where
    pretty (TypeSig name body) =
      PP.text "Type Signature:"
      PP.<$>
      PP.indent 2
        ( PP.text "name:" <+> PP.pretty name
          PP.<$>
          PP.text "body:" <+> PP.pretty body
        )


-- Vow --------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Vow x) where
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
instance PrettyX x => PP.Pretty (Var x) where
  pretty (Var name body) =
    PP.text "Variable Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )


-- Value -------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Val x) where
  pretty (Val name body) =
    PP.text "Value Item:"
    PP.<$>
    PP.indent 2
      ( PP.text "name:" <+> PP.pretty name
        PP.<$>
        PP.text "body:" <+> PP.pretty body
      )


-- Function ---------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Fun x) where
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
instance PrettyX x => PP.Pretty (NewType x) where
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
instance PrettyX x => PP.Pretty (TypeAlias x) where
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
instance PrettyX x => PP.Pretty (TypeClass x) where
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
instance PrettyX x => PP.Pretty (TypeClassInst x) where
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
instance PrettyX x => PP.Pretty (DataType x) where
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
instance BinaryX x => Binary (Expose x) where
  get =
    Expose <$> get
      
  put (Expose n) =
    put n


-- Name ------------------------------------------------------------------------

instance BinaryX x => Binary (Name x) where
    put (Name n ex) =
      put n >> put ex
          
    get =
      Name <$> get <*> get


-- Literal ---------------------------------------------------------------------
instance BinaryX x => Binary (Lit x) where
  get = do
    n <- getWord8
    case n of
      1 -> IntLit <$> get <*> get
      2 -> DblLit <$> get <*> get
      3 -> ChrLit <$> get <*> get
      4 -> StrLit <$> get <*> get
      5 -> BoolLit <$> get <*> get
      6 -> Lit <$> get
      _ -> error "unexpected input"

  put literal =
    case literal of
      IntLit x v    -> putWord8 1 >> put x >> put v
      DblLit x v    -> putWord8 2 >> put x >> put v
      ChrLit x v    -> putWord8 3 >> put x >> put v
      StrLit x v    -> putWord8 4 >> put x >> put v
      BoolLit x v   -> putWord8 5 >> put x >> put v
      Lit x         -> putWord8 6 >> put x



-- Type ------------------------------------------------------------------------
instance BinaryX x => Binary (Type x) where
  put tipe =
    case tipe of
      TyFun ex t1 t2      -> putWord8 1 >> put ex >> put t1 >> put t2
      TyTuple ex ts       -> putWord8 2 >> put ex >> put ts
      TyApp ex tc ta      -> putWord8 3 >> put ex >> put tc >> put ta
      TyVar ex n          -> putWord8 4 >> put ex >> put n
      TyCon ex n          -> putWord8 5 >> put ex >> put n
      Type ex             -> putWord8 6 >> put ex
        
  get =
    do  n <- getWord8
        case n of
          1 -> TyFun <$> get <*> get <*> get
          2 -> TyTuple <$> get <*> get
          3 -> TyApp <$> get <*> get <*> get
          4 -> TyVar <$> get <*> get
          5 -> TyCon <$> get <*> get
          6 -> Type <$> get
          _ -> undefined


instance BinaryX x => Binary (QType x) where
  get =
    QType <$> get <*> get

  put (QType ctx tipe) =
    put ctx >> put tipe


-- Type Context --------------------------------------------------------------------

instance BinaryX x => Binary (TyContext x) where
  get =
    TyContext <$> get

  put (TyContext assrts) =
    put assrts

instance BinaryX x => Binary (TyAssert x) where
  get =
    TyAssert <$> get <*> get

  put (TyAssert con args) =
    put con >> put args

          
-- Expr -------------------------------------------------------------------------
instance BinaryX x => Binary (Exp x) where
  get = do
    n <- getWord8
    case n of
      1 -> ELit <$> get <*> get
      2 -> EVar <$> get <*> get
      3 -> ECon <$> get <*> get

      4 -> EApp <$> get <*> get <*> get
      5 -> EInfixApp <$> get <*> get <*> get <*> get
      
      6 -> ELam <$> get <*> get <*> get
      
      7 -> EDo <$> get <*> get
      8 -> EReturn <$> get <*> get
      9 -> EIf <$> get <*> get <*> get <*> get
      10 -> EWhile <$> get <*> get <*> get

      11 -> EPrim <$> get <*> get <*> get <*> get
      12 -> EBinary <$> get <*> get <*> get <*> get
      13 -> EUnary <$> get <*> get <*> get
      14 -> EAssign <$> get <*> get <*> get <*> get
      
      15 -> ETypeHint <$> get <*> get <*> get
      16 -> EBottom <$> get
      17 -> Exp <$> get
      _ -> undefined
      
  put e =
    case e of
      ELit ex l           -> putWord8 1 >> put ex >> put l
      EVar ex n           -> putWord8 2 >> put ex >> put n
      ECon ex n           -> putWord8 3 >> put ex >> put n

      EApp ex f as        -> putWord8 4 >> put ex >> put f >> put as
      EInfixApp ex l f r  -> putWord8 5 >> put ex >> put l >> put f >> put r
      
      ELam ex p e         -> putWord8 6 >> put ex >> put p >> put e

      EDo ex e            -> putWord8 7 >> put ex >> put e 
      EReturn ex e        -> putWord8 8 >> put ex >> put e
      EIf ex p a b        -> putWord8 9 >> put ex >> put p >> put a >> put b
      EWhile ex c a       -> putWord8 10 >> put ex >> put c >> put a

      EPrim ex i a b      -> putWord8 11 >> put ex >> put i >> put a >> put b
      EBinary ex l o r    -> putWord8 12 >> put ex >> put l >> put o >> put r
      EUnary ex o a       -> putWord8 13 >> put ex >> put o >> put a
      EAssign ex l o r    -> putWord8 14 >> put ex >> put l >> put o >> put r

      ETypeHint ex e t    -> putWord8 15 >> put ex >> put e >> put t
      EBottom ex          -> putWord8 16 >> put ex
      Exp ex              -> putWord8 17 >> put ex

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
instance BinaryX x => Binary (NewType x) where
  get =
    NewType <$> get <*> get <*> get

  put (NewType name tyvars body) =
    put name >> put tyvars >> put body


-- Type Alias ---------------------------------------------------------------------
instance BinaryX x => Binary (TypeAlias x) where
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


makeLenses ''Name
makeLenses ''Fun