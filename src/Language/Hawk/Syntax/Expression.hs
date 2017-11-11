{-# LANGUAGE  DeriveGeneric
            , DeriveFunctor
            , DeriveFoldable
            , DeriveTraversable
            , StandaloneDeriving
            , FlexibleContexts
            , UndecidableInstances
            , TypeFamilies
            , OverloadedStrings
            , LambdaCase
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.Expression where

import Control.Lens
import Data.Aeson
import Data.Binary (Binary)
import Data.Data
import Data.Data.Lens (uniplate)
import Data.Default.Class
import Data.Monoid
import Data.Text (Text)
import GHC.Generics (Generic)

import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Pass
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Type
import Language.Hawk.Syntax.Kind
import Language.Hawk.Syntax.TypeLiteral

import qualified Text.PrettyPrint.Leijen.Text as PP


-- -----------------------------------------------------------------------------
-- | Expression

data Exp
  = EVar  Text
  | EApp  Exp Exp
  | ELam  Text Exp
  | ELet  (Text, Exp) Exp
  | ELit  Lit
  | ECon  Text
  | EPrim PrimInstr
  | EIf   Exp Exp Exp
  | EDup  Exp
  | EFree Text Exp

  -- Hints
  | EType Type Exp
  | ETLit TLit Exp
  | ELoc  Loc Exp
  | EParen Exp
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)



class HasVar a where
  var :: a -> Maybe Text

instance HasVar Exp where
  var = \case
    EVar n -> Just n
    ECon n -> Just n
    _      -> Nothing


instance HasKind Exp where
  kind = \case
    EType t _ -> kind t
    ELoc _ e -> kind e
    EParen e -> kind e
    _ -> error "no kind"


-- -----------------------------------------------------------------------------
-- | Instances

instance Default Exp where
  def = ECon "()"


instance Binary Exp
instance Plated Exp
instance FromJSON Exp
instance ToJSON Exp

-- -----------------------------------------------------------------------------
-- | "Smart" Constructors


var_ :: Text -> Loc -> Exp
var_ n l = ELoc l $ EVar n

lam_ :: (Text, Loc) -> Exp -> Exp
lam_ (b, l1) e@(ELoc l2 _)
  = ELoc (l1<>l2) $ ELam b e


let_ :: [(Text, Exp)] -> Exp -> Exp
let_ bs e = foldr elet' e (reverse bs)
  where
    elet' a@(_, (ELoc l1 _)) b@(ELoc l2 _)
      = ELoc (l1 <> l2) $ ELet a b

eapp_ :: Exp -> [Exp] -> Exp
eapp_ f = foldr eapp' f . reverse
  where
    eapp' b@(ELoc l1 _) a@(ELoc l2 _)
      =  ELoc (l1 <> l2)
              (EApp a b)


mkOp1 :: L Text -> Exp -> Exp
mkOp1 (L l1 name) e@(ELoc l2 _) 
  = ELoc l3 $ EApp v e
  where
    v = ELoc l1 $ EVar name
    l3 = l1 <> l2


mkOp2 :: L Text -> Exp -> Exp -> Exp
mkOp2 (L l0@(Loc fp r1) name) lhs rhs
  = ELoc l2 (EApp (ELoc l1 $ EApp v lhs) rhs)
  where
    v = ELoc l0 $ EVar name
    (ELoc (Loc _ r2) _) = lhs
    (ELoc (Loc _ r3) _) = rhs
    l1 = Loc fp (r1 <> r2)
    l2 = Loc fp (r1 <> r3)

-- -----------------------------------------------------------------------------
-- | Class Instances

instance PP.Pretty Exp where
    pretty (ELit lit) =
      PP.textStrict "Literal:" PP.<+> PP.pretty lit

    pretty (EVar name) =
      PP.textStrict "Variable:" PP.<+> PP.pretty name
      
    pretty (ECon name) =
      PP.textStrict "Con:" PP.<+> PP.pretty name

    pretty (EPrim i) =
      PP.textStrict "Prim:"
      PP.<$>
      PP.indent 2 
        ( PP.textStrict "instruction:" PP.<+> PP.pretty i
        )

    pretty (EApp f as) =
      PP.textStrict "Application:"
      PP.<$>
      PP.indent 2 
        ( PP.textStrict "expression:" PP.<+> PP.pretty f
          PP.<$>
          PP.textStrict "applied to:" PP.<+> PP.pretty as
        )

    pretty (ELam b e) =
      PP.textStrict "Lambda:" 
      PP.<$>
      PP.indent 2
      (
        PP.textStrict "binding:" PP.<+> PP.pretty b
        PP.<$>
        PP.textStrict "expression:" PP.<+> PP.pretty e
      )

    pretty (EIf predicate thenBranch elseBranch) =
      PP.textStrict "If:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "predicate:" PP.<+> PP.pretty predicate
          PP.<$>
          PP.textStrict "then branch:" PP.<+> PP.pretty thenBranch
          PP.<$>
          PP.textStrict "else branch:" PP.<+> PP.pretty elseBranch
        )

    
    pretty (ELet bs e) =
      PP.textStrict "Let:"
      PP.<$>
      PP.indent 2 
        ( PP.textStrict "bindings:" PP.<+> PP.pretty bs
          PP.<$>
          PP.textStrict "in:" PP.<+> PP.pretty e
        )

    pretty (EDup e) =
      PP.textStrict "Dup:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "exp:" PP.<+> PP.pretty e
        )

    pretty (EFree n e) =
      PP.textStrict "Free:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "var:" PP.<+> PP.pretty n
          PP.<$>
          PP.textStrict "so:" PP.<+> PP.pretty e
        )
        
    pretty (EType e t) =
      PP.textStrict "Type Hint:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "expression:" PP.<+> PP.pretty e
          PP.<$>
          PP.textStrict "type:" PP.<+> PP.pretty t
        )

    pretty (ETLit t e) =
      PP.textStrict "TLit Hint:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "tlit:" PP.<+> PP.pretty t
          PP.<$>
          PP.textStrict "exp:" PP.<+> PP.pretty e
        )

    pretty (ELoc loc e) =
      PP.textStrict "Location Hint:"
      PP.<$>
      PP.indent 2
        ( PP.textStrict "location:" PP.<+> PP.pretty loc
          PP.<$>
          PP.textStrict "exp:" PP.<+> PP.pretty e
        )

    pretty (EParen e) =
      PP.parens $ PP.pretty e
