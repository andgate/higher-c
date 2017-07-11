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
           , BangPatterns
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
import Data.Map.Lazy (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Tree
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
import qualified Data.Map.Lazy                as Map
import qualified Data.Set                     as Set

-- -----------------------------------------------------------------------------
-- | Operator

data Operator
  = Op { _opFixity :: Fixity
       , _opPrec   :: Int
       , _opName   :: Text
       } deriving (Show, Data, Typeable, Generic)

instance Eq Operator where
    (==) a b = _opName b == _opName b

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

  | NoConsume Name

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
makeLenses ''Operator

-- -----------------------------------------------------------------------------
-- | Pretty Printing Instances

instance (PP.Pretty l, PP.Pretty r) => PP.Pretty (Either l r) where
    pretty (Left l) =
      PP.pretty l

    pretty (Right r) =
      PP.pretty r 

-- Item ------------------------------------------------------------------------
instance PP.Pretty Item where
    pretty (ForeignItem i) =
      PP.pretty i

    pretty (ExposeItem i) =
      PP.pretty i

    pretty (NoConsume i) =
      PP.textStrict "Non-consumer:" PP.<+> PP.pretty i

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


-- Operator -----------------------------------------------------------------------

instance Binary Operator
instance Binary Fixity


-- Item -----------------------------------------------------------------------
instance Binary Item


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