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
           , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax
  ( module Language.Hawk.Syntax
  , module Language.Hawk.Syntax.Expression
  , module Language.Hawk.Syntax.Extensions
  , module Language.Hawk.Syntax.Literal
  , module Language.Hawk.Syntax.Location
  , module Language.Hawk.Syntax.Name
  , module Language.Hawk.Syntax.Prim
  , module Language.Hawk.Syntax.Type
  , module Language.Hawk.Syntax.TypeLiteral
  ) where

import Control.Lens
import Data.Binary
import Data.Data hiding (Fixity, DataType)
import Data.Data.Lens (uniplate)
import Data.Default.Class
import Data.Either
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text, pack)
import GHC.Types (Constraint)
import GHC.Generics (Generic)
import Language.Hawk.Parse.Lexer.Token
import Language.Hawk.Syntax.Expression
import Language.Hawk.Syntax.Extensions
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Operator
import Language.Hawk.Syntax.Pass
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.TypeLiteral
import Text.PrettyPrint.Leijen.Text ((<+>))


import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set



-- -----------------------------------------------------------------------------
-- | Module

data SrcMod =
  SrcMod
    { _modName :: Text
    , _modSubs :: Map Text SrcMod
    , _modScopes :: Map Text MScope
    } deriving (Show, Eq, Data, Typeable, Generic)

instance Plated SrcMod

-- -----------------------------------------------------------------------------
-- | Module Scope

data MScope =
  MScope
   { _mscopePath      :: FilePath
   , _mscopeTabs      :: MScopeTables
   , _mscopeItems     :: [Item]
   , _mscopeToks      :: [[Token]]
   } deriving (Show, Eq, Data, Typeable, Generic)


data MScopeTables
  = MScopeTables
    { _mscopeDeps   :: [Dependency]
    , _mscopeVars   :: Set Var
    , _mscopeCons   :: Set Con
    , _mscopeTCons  :: Set Con
    , _mscopeOps    :: OpTable
    } deriving (Show, Eq, Data, Typeable, Generic)

-- -----------------------------------------------------------------------------
-- | Operator

type OpTable = Map OpName Operator

data OpName = OpName Text
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

data Operator
  = Op { _opFixity :: Fixity
       , _opPrec :: Integer
       , _opName :: OpName
       } deriving (Show, Eq, Data, Typeable, Generic)

instance Ord Operator where
    compare a b = compare (_opPrec a) (_opPrec b)

data Fixity
  = InfixN
  | InfixL
  | InfixR
  | Prefix
  | Postfix
  deriving (Show, Eq, Data, Typeable, Generic)

-- -----------------------------------------------------------------------------
-- | Item

data Item
  = ForeignItem Foreign
  | ExposeItem Expose

  | DecItem Dec
  | DefItem Def
  
  | NewTyItem NewType
  | TyAliasItem TypeAlias
  
  | DataItem DataType
  | EmptyItem
  deriving (Show, Eq, Data, Typeable, Generic)

instance Default Item where
    def = EmptyItem


-- -----------------------------------------------------------------------------
-- | Dependency

data Dependency =
  Dep
    { _depIsQual  :: Bool
    , _depPath    :: DepPath
    , _depAlias   :: Maybe Text
    } deriving (Eq, Show, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------
-- | Dependency Path

data DepPath = 
    DepPath     Text DepPath
  | DepBase     Text
  | DepSpecify
    { _depSpecIsHidden  :: Bool
    , _depSpecfiers     :: [DepPath]
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------
-- | Foreign

data Foreign =
  Foreign ForeignType Text Dec
  deriving (Show, Eq, Data, Typeable, Generic)


data ForeignType =
  ForeignC
  deriving (Eq, Show, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------
-- | Expose

data Expose =
  Expose Name
  deriving(Show, Eq, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------
-- | Declarations

data Dec
  = Dec
    { _decName :: Name
    , _decType :: Type
    } deriving (Show, Eq, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------
-- | Definitions

data Def
  = Def
    { _defName  :: Name
    , _defPats  :: [Pat]
    , _defBody  :: Exp Var
    } deriving (Show, Eq, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------
-- | Definition Patterns

data Pat
  = Pat
    { _patName  :: Text
    } deriving (Show, Eq, Data, Typeable, Generic)

-- -----------------------------------------------------------------------------
-- | New Type

data NewType
  = NewType
    { _newTyName     :: Name
    , _newTyNewBody  :: Type
    } deriving (Show, Eq, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------
-- | Type Alias

data TypeAlias
    = TypeAlias
      { _tyAliasName   :: Name
      , _tyAliasBody   :: Type
      } deriving (Show, Eq, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------
-- | Data Type

data DataType
    = DataType
      { _dataTyName :: Name
      , _dataTyBody :: [Dec]
      } deriving (Show, Eq, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------
-- | Lens Instances

makeLenses ''Name
makeLenses ''Def
makeClassy ''SrcMod
makeClassy ''MScope
makeClassy ''MScopeTables
makeLenses ''Operator

instance HasMScopeTables MScope where
  mScopeTables = mscopeTabs

-- -----------------------------------------------------------------------------
-- | Pretty Printing Instances

instance (PP.Pretty l, PP.Pretty r) => PP.Pretty (Either l r) where
    pretty (Left l) =
      PP.pretty l

    pretty (Right r) =
      PP.pretty r 



-- Module ------------------------------------------------------------------------
instance PP.Pretty SrcMod where
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
instance PP.Pretty MScope where
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
          PP.pretty (m^.mscopeTabs)
          PP.<$>
          PP.textStrict "Extension:"
          PP.<$>
          PP.indent 2
            (
              PP.pretty (m^.mscopeToks)
            )
        )


instance PP.Pretty MScopeTables where
  pretty mstabs = 
    PP.textStrict "Tables"
    PP.<$>
    PP.indent 2
    ( PP.textStrict "Ops:" <+> (PP.textStrict . pack . show) (mstabs^.mscopeOps)

    )

-- Item ------------------------------------------------------------------------
instance PP.Pretty Item where
    pretty (ForeignItem i) =
      PP.pretty i

    pretty (ExposeItem i) =
      PP.pretty i

    pretty (DecItem i) =
      PP.pretty i

    pretty (DefItem i) =
      PP.pretty i
        
    pretty (NewTyItem i) =
      PP.pretty i

    pretty (TyAliasItem i) =
      PP.pretty i

    pretty (DataItem i) =
      PP.pretty i

    pretty (EmptyItem) =
      PP.textStrict "Empty Item"


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



-- Type Signature ---------------------------------------------------------------
instance PP.Pretty Dec where
    pretty (Dec name tipe) =
      PP.textStrict "Declaration:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "name:" <+> PP.pretty name
          PP.<$>
          PP.textStrict "type:" <+> PP.pretty tipe
        )


-- Function ---------------------------------------------------------------------
instance PP.Pretty Def where
  pretty (Def name pats body) =
    PP.textStrict "Definition:"
    PP.<$>
    PP.indent 2
      ( PP.textStrict "name:" <+> PP.pretty name
        PP.<$>
        PP.textStrict "patterns:" <+> PP.pretty pats
        PP.<$>
        PP.textStrict "body:" <+> PP.pretty body
      )


-- Function Params ----------------------------------------------------------------
instance PP.Pretty Pat where
  pretty (Pat name) =
    PP.textStrict "Pattern:"
    PP.<$>
    PP.indent 2
      ( PP.textStrict "name:" <+> PP.pretty name
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
instance Binary SrcMod

-- MScope -----------------------------------------------------------------------
instance Binary MScope
instance Binary MScopeTables


-- Operator -----------------------------------------------------------------------
instance Binary OpName
instance Binary Operator
instance Binary Fixity


-- Item -----------------------------------------------------------------------
instance Binary Item


-- Dependency ---------------------------------------------------------------
instance Binary Dependency


-- Dependency Path -------------------------------------------------------------
instance Binary DepPath


-- Foreign ------------------------------------------------------------------
instance Binary Foreign


instance Binary ForeignType

-- Expose ---------------------------------------------------------------------
instance Binary Expose


-- Type Signature ---------------------------------------------------------------
instance Binary Dec


-- Function ----------------------------------------------------------------------
instance Binary Def

-- Function Param ----------------------------------------------------------------
instance Binary Pat


-- New Type ----------------------------------------------------------------------
instance Binary NewType


-- Type Alias ---------------------------------------------------------------------
instance Binary TypeAlias


-- Data Type -----------------------------------------------------------------------
instance Binary DataType


-- -----------------------------------------------------------------------------
-- | Helpers


-- Module ---------------------------------------------------------------------

insertModWith :: (SrcMod -> SrcMod -> SrcMod) -> SrcMod -> SrcMod -> SrcMod
insertModWith f child parent
  = parent & modSubs .~ Map.insertWith f (child^.modName) child (parent^.modSubs)


insertModsWith :: (SrcMod -> SrcMod -> SrcMod) -> [SrcMod] -> SrcMod -> SrcMod
insertModsWith _ [] m = m
insertModsWith f children parent
  = foldr (insertModWith f) parent children


unionModWith :: (SrcMod -> SrcMod -> SrcMod) -> (MScope -> MScope -> MScope) -> SrcMod -> SrcMod -> SrcMod
unionModWith f g m1 m2
  = SrcMod { _modName = m1^.modName
        , _modSubs = Map.unionWith f (m1^.modSubs) (m2^.modSubs)
        , _modScopes = Map.unionWith g (m1^.modScopes) (m2^.modScopes)
        }


instance Default SrcMod where
    def
      = SrcMod { _modName = "root"
            , _modSubs = Map.empty
            , _modScopes = Map.empty
            }


instance Monoid SrcMod where
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


instance Default MScope where
    def = mempty

instance Monoid MScope where
    mempty 
      = MScope { _mscopePath      = []
               , _mscopeTabs      = mempty
               , _mscopeItems     = []
               , _mscopeToks      = []
               }
    
    mappend ms1 ms2
      | ms1^.mscopePath /= ms2^.mscopePath
          -- If this ever happens, something went terribly wrong.
          = error "HKC BUG: Can't combined different module scopes"
      | otherwise
          = ms1 { _mscopeTabs  = mappend (ms1^.mscopeTabs) (ms2^.mscopeTabs)
                , _mscopeItems = mappend (ms1^.mscopeItems) (ms2^.mscopeItems)
                , _mscopeToks  = mappend (ms1^.mscopeToks) (ms2^.mscopeToks)
                }

instance Default MScopeTables where
    def
      = MScopeTables
          { _mscopeDeps      = []
          , _mscopeVars      = Set.empty
          , _mscopeCons      = Set.empty
          , _mscopeTCons     = Set.empty
          , _mscopeOps       = Map.empty
          }

instance Monoid MScopeTables where
    mempty = def

    mappend t1 t2
      = t1 { _mscopeDeps      = (t1^.mscopeDeps)       ++      (t2^.mscopeDeps)
           , _mscopeVars      = (t1^.mscopeVars)  `Set.union` (t2^.mscopeVars)
           , _mscopeCons      = (t1^.mscopeCons)  `Set.union` (t2^.mscopeCons)
           , _mscopeTCons     = (t1^.mscopeTCons) `Set.union` (t2^.mscopeTCons)
           , _mscopeOps       = (t1^.mscopeOps)   `Map.union` (t2^.mscopeOps)
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