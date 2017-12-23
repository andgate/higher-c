{-# LANGUAGE  DeriveGeneric
            , FlexibleContexts
            , TypeFamilies
            , OverloadedStrings
            , LambdaCase
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Hawk.Syntax.Term.Source where

import Control.Arrow (first, second)
import Control.Lens
import Data.Aeson
import Data.Binary (Binary)
import Data.Data
import Data.Data.Lens (uniplate)
import Data.Default.Class
import Data.Monoid hiding (Alt)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Subterm

import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Set as Set


-- -----------------------------------------------------------------------------
-- | Terms

type TermName = Name Term Text

type Type = Term

-- Dependent Term
data Term v
  = TVar  v
  | TApp  (Term v) (Term v)
  | TLam  Name (Term v)

  | TPi   (Name, Term v) (Term v)    -- Regular pi, or arrow
  | TLPi   (Name, Term v) (Term v)   -- Linear pi, or lolipop

  | TLet  (Name, Term v) (Term v)
  
  | TLit  Lit
  | TCon  Text
  | TPrim PrimInstr (Term v) (Term v)
  | TIf   (Term v) (Term v) (Term v)
  
  | TDup  v
  | TFree [v] (Term v)

  -- Hints
  | THint  (Term v) (Term v)
  | TSub SubTerm (Term v)
  | TLoc   Loc (Term v)
  | TParen (Term v)

  | TWild
  
  deriving(Eq, Ord, Read, Show, Generic, Data, Typeable)


-- Term instances
instance Binary Term
instance Plated Term
instance FromJSON Term
instance ToJSON Term

-- -----------------------------------------------------------------------------
-- | Default Instances

instance Default Term where
  def = TCon "()"

-- -----------------------------------------------------------------------------
-- | Pretty Instances

instance PP.Pretty Term where
    pretty = \case
      TVar n      -> PP.pretty n
      TApp e1 e2  -> PP.pretty e1 PP.<+> PP.pretty e2
      TLam n e    ->
          PP.textStrict "\\" PP.<> PP.pretty n
            PP.<+> PP.textStrict "->"
            PP.<$> PP.indent 2 (PP.pretty e)
      TLet xs e ->
          PP.textStrict           "let"
            PP.<+> PP.pretty       xs
            PP.<$> PP.textStrict  "in"
            PP.<+> PP.pretty       e
      TLit l      -> PP.pretty l
      TCon n      -> PP.pretty n
      TPrim i a b -> PP.pretty i PP.<+> PP.pretty a PP.<+> PP.pretty b
      TIf e1 e2 e3 ->
          PP.textStrict           "if"
            PP.<+> PP.pretty       e1
            PP.<+> PP.textStrict  "then"
            PP.<+> PP.pretty       e2
            PP.<+> PP.textStrict  "else"
            PP.<+> PP.pretty       e3

      TDup e -> PP.textStrict "dup" PP.<+> PP.pretty e
      TFree n e ->
          PP.textStrict           "free"
            PP.<+> PP.pretty       n
            PP.<+> PP.textStrict  "in"
            PP.<+> PP.pretty       e


      THint t e ->
          PP.pretty               e
            PP.<+> PP.textStrict "::"
            PP.<+> PP.pretty      t


      TLoc l e ->
        PP.pretty               e
            PP.<+> PP.textStrict "@"
            PP.<+> PP.parens (PP.pretty l)

      
      TParen t    -> PP.parens $ PP.pretty t
