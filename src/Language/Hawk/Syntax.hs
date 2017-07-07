{-# LANGUAGE TypeFamilies
           , GADTs
           , DeriveGeneric
           , DataKinds
           , ConstraintKinds
           , KindSignatures
           , EmptyCase
           , StandaloneDeriving
           , TypeOperators
           , PatternSynonyms
           , FlexibleInstances
           , FlexibleContexts
           , OverloadedStrings
           , UndecidableInstances
           , TemplateHaskell
           , LambdaCase
  #-}
module Language.Hawk.Syntax where

import Data.Binary
import Data.Default.Class
import Data.Either
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text, pack)
import Control.Lens
import GHC.Types (Constraint)
import GHC.Generics (Generic, Rec0, (:+:), (:*:))
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Operator
import Language.Hawk.Syntax.Pass
import Language.Hawk.Syntax.Prim
import Text.PrettyPrint.Leijen.Text ((<+>))


import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set


type ForallX (c :: * -> Constraint) (x :: *)
  = ( ForallExp c x
    , c (XMScope x)
    , c (XFun x)
    , c (XFunParam x)
    )

type GenericX (x :: *)
  = ForallX Generic x

type ShowX (x :: *)
  = ForallX Show x

type EqX (x :: *)
  = ForallX Eq x

type OrdX (x :: *)
  = ForallX Ord x

type PrettyX (x :: *)
  = ForallX PP.Pretty x

type BinaryX (x :: *)
  = (ForallX Binary x, GenericX x)

type DefaultX (x :: *)
  = (ForallX Default x)


-- -----------------------------------------------------------------------------
-- | Module

data Mod x =
  Mod
    { _modName :: Text
    , _modSubs :: Map Text (Mod x)
    , _modScopes :: Map Text (MScope x)
    }

deriving instance ShowX x => Show (Mod x)
deriving instance EqX x => Eq (Mod x)
deriving instance GenericX x => Generic (Mod x)

type ModPs = Mod HkcPs
type ModRn = Mod HkcRn
type ModTc = Mod HkcTc
type ModMn = Mod HkcMn

-- -----------------------------------------------------------------------------
-- | Module Scope

data MScope x =
  MScope
   { _mscopePath      :: FilePath
   , _mscopeTabs      :: MScopeTables
   , _mscopeItems     :: [Item x]
   , _mscopeX         :: XMScope x
   }

type family XMScope x

type instance XMScope HkcPs = [[Token]]
type instance XMScope HkcRn = ()
type instance XMScope HkcTc = ()
type instance XMScope HkcMn = ()

deriving instance ShowX x => Show (MScope x)
deriving instance EqX x => Eq (MScope x)
deriving instance GenericX x => Generic (MScope x)

type MScopePs = MScope HkcPs
type MScopeRn = MScope HkcRn
type MScopeTc = MScope HkcTc
type MScopeMn = MScope HkcMn


data MScopeTables
  = MScopeTables
    { _mscopeDeps      :: [Dependency]
    , _mscopeVars      :: Set Var
    , _mscopeTypes     :: Set Name
    , _mscopeVarTypes  :: Map Var Type
    , _mscopeOps       :: Map Var Operator
    } deriving (Show, Eq, Generic)

-- -----------------------------------------------------------------------------
-- | Operator


data Operator
  = Op { _opPrec :: Int
       , _opFixity :: Fixity
       } deriving (Show, Eq, Generic)

data Fixity
  = InfixN
  | InfixL
  | InfixR
  | Prefix
  | Postfix
  deriving (Show, Eq, Generic)

-- -----------------------------------------------------------------------------
-- | Item

data Item x
  = ForeignItem Foreign
  | ExposeItem Expose

  | SigItem TypeSig
  | FunItem (Fun x)
  
  | NewTyItem NewType
  | TyAliasItem TypeAlias
  
  | DataItem DataType
  | EmptyItem


deriving instance ShowX x => Show (Item x)
deriving instance EqX x => Eq (Item x)
deriving instance GenericX x => Generic (Item x)

instance Default (Item x) where
    def = EmptyItem

type ItemPs = Item HkcPs
type ItemRn = Item HkcRn
type ItemTc = Item HkcTc
type ItemMn = Item HkcMn


-- -----------------------------------------------------------------------------
-- | Dependency

data Dependency =
  Dep
    { _depIsQual  :: Bool
    , _depPath    :: DepPath
    , _depAlias   :: Maybe Text
    } deriving (Eq, Show, Generic)


-- -----------------------------------------------------------------------------
-- | Dependency Path

data DepPath = 
    DepPath     Text DepPath
  | DepBase     Text
  | DepSpecify
    { _depSpecIsHidden  :: Bool
    , _depSpecfiers     :: [DepPath]
    }
  deriving (Eq, Show, Generic)


-- -----------------------------------------------------------------------------
-- | Foreign

data Foreign =
  Foreign ForeignType Text TypeSig
  deriving (Show, Eq, Generic)


data ForeignType =
  ForeignC
  deriving (Eq, Show, Generic)


-- -----------------------------------------------------------------------------
-- | Expose

data Expose =
  Expose Name
  deriving(Show, Eq, Generic)


-- -----------------------------------------------------------------------------
-- | Name

data Name
  = Name 
    { _nameText :: Text
    } deriving (Show, Eq, Ord, Generic)



-- -----------------------------------------------------------------------------
-- | Literal

data Lit
  = IntLit Integer
  | FloatLit Double
  | CharLit Char
  | BoolLit Bool
  deriving (Show, Eq, Generic)


-- -----------------------------------------------------------------------------
-- | Type Literal

data TLit
  = TLitInt
  | TLitFloat
  | TLitChar
  | TLitBool
  | TLitData Name
  | TLitFun [TLit] TLit
  deriving (Show, Eq, Generic)


-- -----------------------------------------------------------------------------
-- | Type

data TVar = TypeVar Text deriving (Eq, Ord, Show, Generic)

data Type
  = TCon Name
  | TVar TVar
  | TFun Type Type
  deriving (Show, Eq, Generic)

instance Default Type where
  def = TCon $ Name "()"


-- -----------------------------------------------------------------------------
-- | Expression

data Var = Var Text
  deriving (Eq, Ord, Show, Generic)

data Exp x
  = ELit (XELit x) Lit
  | EVar (XEVar x) Var
  | ECon (XECon x) Var
  | EPrim (XEPrim x) PrimInstr
  | EApp (XEApp x) (Exp x) (Exp x)
  | ELam (XELam x) Var (Exp x)
  | EIf (XEIf x) (Exp x) (Exp x) (Exp x)
  | ELet (XELet x) Var (Exp x) (Exp x)
  | EDup (XEDup x) (Exp x)
  | EDrop (XEDrop x) Var (Exp x)


  -- Type hints and bottom
  | ETypeHint (XETypeHint x) (Exp x) Type
  | Exp (XExp x)

type family XELit x
type family XEVar x
type family XECon x
type family XEApp x
type family XELam x
type family XELet x
type family XEDup x
type family XEDrop x
type family XEPrim x
type family XEIf x
type family XETypeHint x
type family XExp x


type ForallExp (c :: * -> Constraint) (x :: *) =
  ( c (XELit x)
  , c (XEVar x)
  , c (XECon x)
  , c (XEPrim x)
  , c (XEApp x)
  , c (XELam x)
  , c (XEIf x)
  , c (XELet x)
  , c (XEDup x)
  , c (XEDrop x)
  , c (XETypeHint x)
  , c (XExp x)
  )

deriving instance ShowX x => Show (Exp x)
deriving instance EqX x => Eq (Exp x)
deriving instance GenericX x => Generic (Exp x)

instance DefaultX x => Default (Exp x) where
  def = ECon def $ Var "()"

type instance XELit         HkcPs = ()
type instance XEVar         HkcPs = ()
type instance XECon         HkcPs = ()
type instance XEPrim        HkcPs = ()
type instance XEApp         HkcPs = ()
type instance XELam         HkcPs = ()
type instance XEIf          HkcPs = ()
type instance XELet         HkcPs = ()
type instance XEDup         HkcPs = ()
type instance XEDrop        HkcPs = ()
type instance XETypeHint    HkcPs = ()
type instance XExp          HkcPs = ()

type instance XELit         HkcRn = Maybe Location
type instance XEVar         HkcRn = Maybe Location
type instance XECon         HkcRn = Maybe Location
type instance XEPrim        HkcRn = Maybe Location
type instance XEApp         HkcRn = Maybe Location
type instance XELam         HkcRn = Maybe Location
type instance XEIf          HkcRn = Maybe Location
type instance XELet         HkcRn = Maybe Location
type instance XEDup         HkcRn = Maybe Location
type instance XEDrop        HkcRn = Maybe Location
type instance XETypeHint    HkcRn = Maybe Location
type instance XExp          HkcRn = Maybe Location

type instance XELit         HkcTc = (Type, Maybe Location)
type instance XEVar         HkcTc = (Type, Maybe Location)
type instance XECon         HkcTc = (Type, Maybe Location)
type instance XEPrim        HkcTc = (Type, Maybe Location)
type instance XEApp         HkcTc = (Type, Maybe Location)
type instance XELam         HkcTc = (Type, Maybe Location)
type instance XEIf          HkcTc = (Type, Maybe Location)
type instance XELet         HkcTc = (Type, Maybe Location)
type instance XEDup         HkcTc = (Type, Maybe Location)
type instance XEDrop        HkcTc = (Type, Maybe Location)
type instance XETypeHint    HkcTc = (Type, Maybe Location)
type instance XExp          HkcTc = ()


type instance XELit         HkcMn = ()
type instance XEVar         HkcMn = TLit
type instance XECon         HkcMn = ()
type instance XEPrim        HkcMn = ()
type instance XEApp         HkcMn = TLit
type instance XELam         HkcMn = ()
type instance XEIf          HkcMn = TLit
type instance XELet         HkcMn = TLit
type instance XEDup         HkcMn = ()
type instance XEDrop        HkcMn = ()
type instance XETypeHint    HkcMn = ()
type instance XExp          HkcMn = ()

type ExpPs = Exp HkcPs
type ExpRn = Exp HkcRn
type ExpTc = Exp HkcTc
type ExpMn = Exp HkcMn

-- -----------------------------------------------------------------------------
-- | Type Signature

data TypeSig
  = TypeSig
    { _tySigName :: Name
    , _tySigBody :: Type
    } deriving (Show, Eq, Generic)


-- -----------------------------------------------------------------------------
-- | Function

data Fun x
  = Fun
    { _funName   :: Name
    , _funParams :: [FunParam x]
    , _funBody   :: Exp x
    , _funX      :: XFun x
    }

deriving instance ShowX x => Show (Fun x)
deriving instance EqX x => Eq (Fun x)
deriving instance GenericX x => Generic (Fun x)

type family XFun x

type instance XFun HkcPs = ()
type instance XFun HkcRn = ()
type instance XFun HkcTc = ()
type instance XFun HkcMn = TLit


type FunMn = Fun HkcMn


-- -----------------------------------------------------------------------------
-- | Function Param

data FunParam x
  = FunParam
    { _fparamName :: Text
    , _fparamX :: XFunParam x
    }

deriving instance ShowX x => Show (FunParam x)
deriving instance EqX x => Eq (FunParam x)
deriving instance GenericX x => Generic (FunParam x)

type family XFunParam x

type instance XFunParam HkcPs = ()
type instance XFunParam HkcRn = ()
type instance XFunParam HkcTc = ()
type instance XFunParam HkcMn = TLit

type FunParamMn = FunParam HkcMn

-- -----------------------------------------------------------------------------
-- | New Type

data NewType
  = NewType
    { _newTyName     :: Name
    , _newTyNewBody  :: Type
    } deriving (Show, Eq, Generic)


-- -----------------------------------------------------------------------------
-- | Type Alias

data TypeAlias
    = TypeAlias
      { _tyAliasName   :: Name
      , _tyAliasBody   :: Type
      } deriving (Show, Eq, Generic)


-- -----------------------------------------------------------------------------
-- | Data Type

data DataType
    = DataType
      { _dataTyName :: Name
      , _dataTyBody :: [TypeSig]
      } deriving (Show, Eq, Generic)


-- -----------------------------------------------------------------------------
-- | Lens Instances

makeLenses ''Name
makeLenses ''Fun
makeLenses ''Mod
makeLenses ''MScope
makeLenses ''MScopeTables
makeLenses ''Operator

-- -----------------------------------------------------------------------------
-- | Pretty Printing Instances

instance (PP.Pretty l, PP.Pretty r) => PP.Pretty (Either l r) where
    pretty (Left l) =
      PP.pretty l

    pretty (Right r) =
      PP.pretty r 



-- Module ------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Mod x) where
    pretty m =
      PP.textStrict "Module:" PP.<+> PP.textStrict (m^.modName)
      PP.<$>
      PP.indent 2
        ( PP.textStrict "Sub Modules:"
          PP.<$>
          PP.indent 2
            (
              PP.pretty (Map.elems $ (m^.modSubs))
            )
          PP.<$>
          PP.textStrict "Module Scopes:"
          PP.<$>
          PP.indent 2
            (
              PP.pretty (Map.elems $ (m^.modScopes))
            )
        )


-- ModuleScope ------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (MScope x) where
    pretty m =
      PP.textStrict "Module Scope:" PP.<+> PP.textStrict (pack (m^.mscopePath))
      PP.<$>
      PP.indent 2
        ( PP.textStrict "Items:"
          PP.<$>
          PP.indent 2
            (
              PP.pretty (m^.mscopeItems)
            )
          PP.<$>
          PP.textStrict "Extension:"
          PP.<$>
          PP.indent 2
            (
              PP.pretty (m^.mscopeX)
            )
        )


-- Item ------------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Item x) where
    pretty (ForeignItem i) =
      PP.pretty i

    pretty (ExposeItem i) =
      PP.pretty i

    pretty (SigItem i) =
      PP.pretty i

    pretty (FunItem i) =
      PP.pretty i
        
    pretty (NewTyItem i) =
      PP.pretty i

    pretty (TyAliasItem i) =
      PP.pretty i

    pretty (DataItem i) =
      PP.pretty i


-- Dependency ------------------------------------------------------------------
instance PP.Pretty Dependency where
    pretty (Dep ql p a) =
      PP.textStrict "Dependency:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "Is Qualified:" PP.<+> PP.pretty ql
          PP.<$>
          PP.textStrict "Path:" PP.<+> PP.pretty p
          PP.<$>
          PP.textStrict "Alias:" PP.<+> PP.pretty a
        )


-- Foreign ------------------------------------------------------------------
instance PP.Pretty Foreign where
    pretty (Foreign ft n fs) =
      PP.textStrict "Foreign:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "Foreign Type:" PP.<+> PP.pretty ft
          PP.<$>
          PP.textStrict "Foreign Name:" PP.<+> PP.pretty n
          PP.<$>
          PP.textStrict "Foreign Sig:" PP.<+> PP.pretty fs
        )

instance PP.Pretty ForeignType where
    pretty ForeignC =
      PP.textStrict "ForeignC"


-- Expose --------------------------------------------------------------------
instance PP.Pretty Expose where
    pretty (Expose n) =
      PP.textStrict "Expose:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "name:" PP.<+> PP.pretty n
        )


-- Dependency Path ------------------------------------------------------------
instance PP.Pretty DepPath where
    pretty (DepPath n r) =
      PP.textStrict n PP.<> PP.textStrict "."  PP.<> PP.pretty r
        
    pretty (DepBase n) =
      PP.textStrict n
        
    pretty (DepSpecify False rs) =
      PP.textStrict "(" PP.<> PP.pretty rs PP.<> PP.textStrict ")"

    pretty (DepSpecify True rs) =
      PP.textStrict "(\\" PP.<> PP.pretty rs PP.<> PP.textStrict ")"


-- Name ------------------------------------------------------------------------

instance PP.Pretty Name where
    pretty (Name n) =
      PP.textStrict n


-- Literal ------------------------------------------------------------------------

instance PP.Pretty Lit where
  pretty = \case
    IntLit v ->
      PP.pretty v
        
    FloatLit v ->
      PP.pretty v
    
    CharLit c ->
      PP.squotes $ PP.pretty c
    
    BoolLit v ->
      PP.pretty v


-- Type ------------------------------------------------------------------------
instance PP.Pretty TVar where
  pretty (TypeVar x) =
    PP.textStrict "TVar:" <+> PP.dquotes (PP.pretty x)

instance PP.Pretty Type where
    pretty = \case
      TCon n ->
        PP.textStrict "TCon:" <+> PP.pretty n

      TVar tvar ->
        PP.pretty tvar

      TFun f x ->
        PP.textStrict "TFun:"
        PP.<$>
        PP.indent 2
          ( PP.textStrict "func:" <+> PP.pretty f
            PP.<$>
            PP.textStrict "arg:" PP.<$> PP.pretty x
          )


-- Expr -------------------------------------------------------------------------
instance PP.Pretty Var where
  pretty (Var n) =
    PP.textStrict "Var:" PP.<+> PP.textStrict n

instance PrettyX x => PP.Pretty (Exp x) where
    pretty (ELit ext lit) =
      PP.textStrict "Literal:" PP.<+> PP.pretty lit
      PP.<$>
      PP.textStrict "ext:" <+> PP.pretty ext

    pretty (EVar ext name) =
      PP.textStrict "Variable:" PP.<+> PP.pretty name
      PP.<$>
      PP.textStrict "ext:" <+> PP.pretty ext
      
    pretty (ECon ext name) =
      PP.textStrict "Con:" PP.<+> PP.pretty name
      PP.<$>
      PP.textStrict "ext:" <+> PP.pretty ext

    pretty (EPrim ext i) =
      PP.textStrict "Prim:"
      PP.<$>
      PP.indent 2 
        ( PP.textStrict "instruction:" <+> PP.pretty i
          PP.<$>
          PP.textStrict "ext:" <+> PP.pretty ext
        )

    pretty (EApp ext f as) =
      PP.textStrict "Application:"
      PP.<$>
      PP.indent 2 
        ( PP.textStrict "expression:" <+> PP.pretty f
          PP.<$>
          PP.textStrict "applied to:" <+> PP.pretty as
          PP.<$>
          PP.textStrict "ext:" <+> PP.pretty ext
        )

    pretty (ELam ext ps b) =
      PP.textStrict "Lambda:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "params:" <+> PP.pretty ps
          PP.<$>
          PP.textStrict "body:" <+> PP.pretty b
          PP.<$>
          PP.textStrict "ext:" <+> PP.pretty ext
        )

    pretty (EIf ext predicate thenBranch elseBranch) =
      PP.textStrict "If:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "predicate:" <+> PP.pretty predicate
          PP.<$>
          PP.textStrict "then branch:" <+> PP.pretty thenBranch
          PP.<$>
          PP.textStrict "else branch:" <+> PP.pretty elseBranch
          PP.<$>
          PP.textStrict "ext:" <+> PP.pretty ext
        )

    pretty (ELet ext n lhs e) =
      PP.textStrict "Let:"
      PP.<$>
      PP.indent 2 
        ( PP.textStrict "name:" <+> PP.pretty n
          PP.<$>
          PP.textStrict "lhs:" <+> PP.pretty lhs
          PP.<$>
          PP.textStrict "exp:" <+> PP.pretty e
          PP.<$>
          PP.textStrict "ext:" <+> PP.pretty ext
        )

    pretty (EDup ext e) =
      PP.textStrict "Dup:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "exp:" <+> PP.pretty e
          PP.<$>
          PP.textStrict "ext:" <+> PP.pretty ext
        )

    pretty (EDrop ext n e) =
      PP.textStrict "Drop:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "var:" <+> PP.pretty n
          PP.<$>
          PP.textStrict "in:" <+> PP.pretty e
          PP.<$>
          PP.textStrict "ext:" <+> PP.pretty ext
        )
        
    pretty (ETypeHint ext e t) =
      PP.textStrict "Type Hint:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "expression:" <+> PP.pretty e
          PP.<$>
          PP.textStrict "hint:" <+> PP.pretty t
          PP.<$>
          PP.textStrict "ext:" <+> PP.pretty ext
        )


    pretty (Exp ext) =
      PP.textStrict "ext:" <+> PP.pretty ext


-- Type Signature ---------------------------------------------------------------
instance PP.Pretty TypeSig where
    pretty (TypeSig name body) =
      PP.textStrict "Type Signature:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "name:" <+> PP.pretty name
          PP.<$>
          PP.textStrict "body:" <+> PP.pretty body
        )


-- Function ---------------------------------------------------------------------
instance PrettyX x => PP.Pretty (Fun x) where
  pretty (Fun name params body x) =
    PP.textStrict "Function Item:"
    PP.<$>
    PP.indent 2
      ( PP.textStrict "name:" <+> PP.pretty name
        PP.<$>
        PP.textStrict "params:" <+> PP.pretty params
        PP.<$>
        PP.textStrict "body:" <+> PP.pretty body
        PP.<$>
        PP.textStrict "ext:" <+> PP.pretty x
      )


-- Function Params ----------------------------------------------------------------
instance PrettyX x => PP.Pretty (FunParam x) where
  pretty (FunParam name x) =
    PP.textStrict "Function Param:"
    PP.<$>
    PP.indent 2
      ( PP.textStrict "name:" <+> PP.pretty name
        PP.<$>
        PP.textStrict "ext:" <+> PP.pretty x
      )


-- New Type ----------------------------------------------------------------------
instance PP.Pretty NewType where
    pretty (NewType name body) =
      PP.textStrict "New Type:"
      PP.<$>
      PP.indent 2
        ( 
          PP.textStrict "name:" <+> PP.pretty name
          PP.<$>
          PP.textStrict "body:" <+> PP.pretty body
        )

-- Type Alias ---------------------------------------------------------------------
instance PP.Pretty TypeAlias where
    pretty (TypeAlias name body) =
      PP.textStrict "Type Alias:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "name:" <+> PP.pretty name
          PP.<$>
          PP.textStrict "body:" <+> PP.pretty body
        )


-- Data Type -----------------------------------------------------------------------
instance PP.Pretty DataType where
    pretty (DataType name body) =
      PP.textStrict "Data Type:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "name:" <+> PP.pretty name
          PP.<$>
          PP.textStrict "body:" <+> PP.pretty body
        )


-- -----------------------------------------------------------------------------
-- | Binary Instances

-- Module -----------------------------------------------------------------------
instance BinaryX x => Binary (Mod x)

-- MScope -----------------------------------------------------------------------
instance BinaryX x => Binary (MScope x)
instance Binary MScopeTables

-- Operator -----------------------------------------------------------------------
instance Binary Operator
instance Binary Fixity

-- Item -----------------------------------------------------------------------
instance BinaryX x => Binary (Item x)


-- Dependency ---------------------------------------------------------------
instance Binary Dependency


-- Dependency Path -------------------------------------------------------------
instance Binary DepPath


-- Foreign ------------------------------------------------------------------
instance Binary Foreign


instance Binary ForeignType

-- Expose ---------------------------------------------------------------------
instance Binary Expose


-- Name ------------------------------------------------------------------------

instance Binary Name


-- Literal ---------------------------------------------------------------------
instance Binary Lit

-- Type Literals ------------------------------------------------------------------------
instance Binary TLit

-- Type ------------------------------------------------------------------------
instance Binary TVar
instance Binary Type where

          
-- Expr -------------------------------------------------------------------------
instance Binary Var
instance BinaryX x => Binary (Exp x)


-- Type Signature ---------------------------------------------------------------
instance Binary TypeSig


-- Function ----------------------------------------------------------------------
instance BinaryX x => Binary (Fun x)

-- Function Param ----------------------------------------------------------------
instance BinaryX x => Binary (FunParam x)


-- New Type ----------------------------------------------------------------------
instance Binary NewType


-- Type Alias ---------------------------------------------------------------------
instance Binary TypeAlias


-- Data Type -----------------------------------------------------------------------
instance Binary DataType


-- -----------------------------------------------------------------------------
-- | Helpers


-- Module ---------------------------------------------------------------------

-- Construction function for parse
mkModPs :: FilePath -> [Text] -> [Either ModPs [Token]] -> ModPs
mkModPs fp [] _ = error "Cannot make empty module"
mkModPs fp [n] xs
  = insertModsWith mappend subs m
  where
    subs = lefts xs
    items = rights xs

    m = Mod { _modName = n
            , _modSubs = mempty
            , _modScopes = Map.singleton (pack fp) ms
            }

    ms = (mempty :: MScopePs)
            { _mscopePath  = fp
            , _mscopeItems = mempty
            , _mscopeX     = items
            }
mkModPs fp (n:ns) xs
  = Mod { _modName = n
        , _modSubs = Map.singleton (head ns) (mkModPs fp ns xs)
        , _modScopes = mempty
        }


insertModWith :: (Mod x -> Mod x -> Mod x) -> Mod x -> Mod x -> Mod x
insertModWith f child parent
  = parent & modSubs .~ Map.insertWith f (child^.modName) child (parent^.modSubs)


insertModsWith :: (Mod x -> Mod x -> Mod x) -> [Mod x] -> Mod x -> Mod x
insertModsWith _ [] m = m
insertModsWith f children parent
  = foldr (insertModWith f) parent children


unionModWith :: (Mod x -> Mod x -> Mod x) -> (MScope x -> MScope x -> MScope x) -> Mod x -> Mod x -> Mod x
unionModWith f g m1 m2
  = Mod { _modName = m1^.modName
        , _modSubs = Map.unionWith f (m1^.modSubs) (m2^.modSubs)
        , _modScopes = Map.unionWith g (m1^.modScopes) (m2^.modScopes)
        }


instance Default (Mod x) where
    def
      = Mod { _modName = "root"
            , _modSubs = Map.empty
            , _modScopes = Map.empty
            }


instance Monoid (XMScope x) => Monoid (Mod x) where
    mempty = def

    mappend m1 m2
      | m1^.modName == "root" && m2^.modName /= "root"
        = insertModWith mappend m2 m1

      | m1^.modName /= "root" && m2^.modName == "root"
        = insertModWith mappend m1 m2

      | m1^.modName == "root" && m2^.modName == "root"
        = unionModWith mappend mappend m1 m2

      | otherwise
        = mappend m2 . mappend m1 $ mempty


instance Monoid (XMScope x) => Default (MScope x) where
    def = mempty

instance Monoid (XMScope x) => Monoid (MScope x) where
    mempty 
      = MScope { _mscopePath      = []
               , _mscopeTabs      = mempty
               , _mscopeItems     = []
               , _mscopeX         = mempty
               }
    
    mappend ms1 ms2
      | ms1^.mscopePath /= ms2^.mscopePath
          -- If this ever happens, something went terribly wrong.
          = error "HKC BUG: Can't combined different module scopes"
      | otherwise
          = ms1 { _mscopeTabs  = mappend (ms1^.mscopeTabs) (ms2^.mscopeTabs)
                , _mscopeItems = mappend (ms1^.mscopeItems) (ms2^.mscopeItems)
                , _mscopeX     = mappend (ms1^.mscopeX) (ms2^.mscopeX)
                }

instance Default MScopeTables where
    def
      = MScopeTables
          { _mscopeDeps      = []
          , _mscopeVars      = Set.empty
          , _mscopeTypes     = Set.empty
          , _mscopeVarTypes  = Map.empty
          , _mscopeOps       = Map.empty
          }

instance Monoid MScopeTables where
    mempty = def

    mappend t1 t2
      = t1 { _mscopeDeps      = (t1^.mscopeDeps)         ++      (t2^.mscopeDeps)
           , _mscopeVars      = (t1^.mscopeVars)     `Set.union` (t2^.mscopeVars)
           , _mscopeTypes     = (t1^.mscopeTypes)    `Set.union` (t2^.mscopeTypes)
           , _mscopeVarTypes  = (t1^.mscopeVarTypes) `Map.union` (t2^.mscopeVarTypes)
           , _mscopeOps       = (t1^.mscopeOps)      `Map.union` (t2^.mscopeOps)
           }



-- Type Helpers --------------------------------------------------------------

-- Smart Constructors


-- Expression Helpers --------------------------------------------------------

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