{-# LANGUAGE  DeriveGeneric
            , FlexibleContexts
            , TypeFamilies
            , OverloadedStrings
            , LambdaCase
            , TemplateHaskell
  #-}
module Language.Hawk.Syntax.Term.Source where

import Control.Arrow (first, second)
import Data.Default.Class
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)

import Language.Hawk.Syntax.Definition.Source
import Language.Hawk.Syntax.Literal
import Language.Hawk.Syntax.Location
import Language.Hawk.Syntax.Name
import Language.Hawk.Syntax.Pattern.Source
import Language.Hawk.Syntax.Prim
import Language.Hawk.Syntax.Subterm

import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Set as Set


-- -----------------------------------------------------------------------------
-- | Terms

type Type = Term

-- Dependent Term
data Term v
  = TVar  v
  | TLit  Lit
  
  | TCon  Text
  | TPrim PrimInstr (Term v) (Term v)
  
  | TApp  (Term v) (Term v)
  | TLam  (Pat (Type v) v) (Term v)

  | TPi   (Pat (Type v) v) (Term v)   -- Regular pi, or arrow
  | TLPi  (Pat (Type v) v) (Term v)   -- Linear pi, or lolipop
  
  | TLet  [(Loc, Def (Term v))] (Term v)
  | TCase  (Term v) [(Pat (Term v) v, Term v)]
  
  | TDup  v
  | TFree [v] (Term v)

  -- Annotations
  | TAnnot (Term v) (Type v)
  | TSub   Subterm (Term v)
  | TLoc   Loc (Term v)
  | TParen (Term v)
  | TWild
  deriving(Show)


-- -----------------------------------------------------------------------------
-- | Default Instances

instance Default (Term v) where
  def = TCon "()"

-- -----------------------------------------------------------------------------
-- | Pretty Instances

instance (PP.Pretty v) => PP.Pretty (Term v) where
    pretty = \case
      TVar n      -> PP.pretty n
      TLit l      -> PP.pretty l
      
      TCon n      -> PP.pretty n
      TPrim i a b -> PP.pretty i PP.<+> PP.pretty a PP.<+> PP.pretty b
      
      TApp e1 e2  -> PP.pretty e1 PP.<+> PP.pretty e2
      TLam p e    ->
          PP.textStrict "\\" PP.<+> PP.pretty p
            PP.<+> PP.textStrict "."
            PP.<$> PP.indent 2 (PP.pretty e)

      TPi p t    ->
          PP.parens (PP.pretty p)
            PP.<+> PP.textStrict "->"
            PP.<+> PP.pretty t

      TLPi p t   ->
          PP.parens (PP.pretty p)
            PP.<+> PP.textStrict "->"
            PP.<+> PP.pretty t
      
      TLet xs e ->
        PP.textStrict "let"
          PP.<$>
          PP.indent 2 ( PP.vsep $ (PP.pretty . snd) <$> xs )
          PP.<$>
          PP.textStrict  "in" PP.<> PP.indent 2 (PP.pretty e)
      
      TCase e brs ->
          PP.textStrict           "case"
            PP.<+> PP.pretty       e
            PP.<+> PP.textStrict  "of"
            PP.<$> 
            PP.indent 2
              (PP.vsep
                [ PP.pretty p 
                    PP.<+> PP.textStrict "->" 
                    PP.<+> PP.pretty br
                  | (p, br) <- brs
                ]
              )

      TDup e -> PP.textStrict "dup" PP.<+> PP.pretty e
      TFree ns e ->
          PP.textStrict           "free"
            PP.<+> PP.hsep (PP.pretty <$> ns)
            PP.<+> PP.textStrict  "in"
            PP.<+> PP.pretty       e


      TAnnot e t ->
          PP.pretty               e
            PP.<+> PP.textStrict ":"
            PP.<+> PP.pretty      t
      
      TSub st e ->
        PP.parens 
          ( PP.pretty e 
              PP.<+> PP.textStrict "?"
              PP.<+> PP.pretty st
          )
      
      TLoc _ e  -> PP.pretty e -- ignore location
      TParen t  -> PP.parens $ PP.pretty t
      TWild     -> PP.textStrict "_"