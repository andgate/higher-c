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


instance HasType Exp where
  typeof = \case
    EType t _  -> t
    ELoc _ e   -> typeof e
    EParen e   -> typeof e

    --ELit l -> typeof l
    --EPrim i -> typeof i
    
    _ -> error "Cannot find type of untyped expression."


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



unlocate :: Exp -> Exp
unlocate = transform $ \case
  ELoc _ e -> e
  e -> e
  

untype :: Exp -> Exp
untype = transform $ \case
  EType _ e -> e
  e -> e


-- -----------------------------------------------------------------------------
-- | Class Instances

instance PP.Pretty Exp where
    pretty = \case
      EVar n      -> PP.pretty n
      EApp e1 e2  -> PP.pretty e1 PP.<+> PP.pretty e2
      ELam n e    ->
          PP.textStrict "\\" PP.<> PP.pretty n
            PP.<+> PP.textStrict "->"
            PP.<$> PP.indent 2 (PP.pretty e)
      ELet (n, e1) e2 ->
          PP.textStrict           "let"
            PP.<+> PP.pretty       n
            PP.<+> PP.textStrict  "="
            PP.<+> PP.align (PP.pretty  e1)
            PP.<$> PP.textStrict  "in"
            PP.<+> PP.pretty       e2
      ELit l      -> PP.pretty l
      ECon n      -> PP.pretty n
      EPrim i     -> PP.pretty i
      EIf e1 e2 e3 ->
          PP.textStrict           "if"
            PP.<+> PP.pretty       e1
            PP.<+> PP.textStrict  "then"
            PP.<+> PP.pretty       e2
            PP.<+> PP.textStrict  "else"
            PP.<+> PP.pretty       e3

      EDup e -> PP.textStrict "dup" PP.<+> PP.pretty e
      EFree n e ->
          PP.textStrict           "free"
            PP.<+> PP.pretty       n
            PP.<+> PP.textStrict  "in"
            PP.<+> PP.pretty       e


      EType t e ->
          PP.pretty               e
            PP.<+> PP.textStrict "::"
            PP.<+> PP.pretty      t


      ELoc l e ->
        PP.pretty               e
            PP.<+> PP.textStrict "@"
            PP.<+> PP.parens (PP.pretty l)

      
      EParen e    -> PP.parens $ PP.pretty e
