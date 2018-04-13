{-# LANGUAGE  DeriveGeneric
            , FlexibleContexts
            , OverloadedStrings
            , LambdaCase
  #-}
module Language.Hawk.Syntax.Source
  ( module Language.Hawk.Syntax.Source
  , module X
  )
  where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc

import Language.Hawk.Syntax.Location as X
import Language.Hawk.Syntax.Name as X
import Language.Hawk.Syntax.Prim as X

import qualified Data.List.NonEmpty             as NE


-- -----------------------------------------------------------------------------
-- | Top Level Definition

data TopLevelDef
  = TopLevelDef         Def
  | TopLevelSig         Sig
  | TopLevelDataDef     DataDef
  | TopLevelClassDef    ClassDef
  | TopLevelInstDef     InstDef
  | TopLevelForeignDef  Foreign
  | TopLevelFixityDef   Fixity
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Definition

data Def
  = Def Text Clause
  deriving (Show)

data Clause
  = Clause [(Plicity, Pat)] Term
  deriving (Show)

data Sig
  = Sig Text Type
  deriving (Show)



------------------------------------------------------------------------
-- Data Definition

data DataDef
  = DataDef Text [(Plicity, Text, Type)] [ConstrDef]
  deriving (Show)


data ConstrDef
  = ConstrDef Text Type
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Class Definition

data ClassDef
  = ClassDef Text [(Plicity, Text, Type)] [MethodDef]
  deriving (Show)

data MethodDef
  = MethodDef Loc Text Term
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Instance Definition

data InstDef = InstDef Type [(Loc, Def)]
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Fixity Declarations

-- A fixity declaration
data Fixity = Fixity FixityKind (L Int) [L Text]
  deriving (Show, Eq)

-- The kinds of fixity
data FixityKind
  = InfixN
  | InfixL
  | InfixR
  | Prefix
  | Postfix
  deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- | Foreign Declarations


data Foreign
  = ForeignImport ForeignType (L Text) (L Text) Type
  | ForeignExport ForeignType (L Text)
  deriving (Show)


data ForeignType =
  ForeignC
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Terms

-- | Plicitness of terms and patterns
data Plicity
  = Implicit | Explicit | Constraint
  deriving (Eq, Ord, Show)


-- Dependent Term
data Term
  = TVar  Text
  | TVal  Value
  
  | TCon  Text
  | TPrim PrimInstr Term Term
  
  | TApp  Term Plicity Term
  | TLam  Plicity Pat Term

  | TPi   Plicity Pat Term   -- Regular pi, or arrow
  | TLPi  Plicity Pat Term   -- Linear pi, or lolipop
  
  | TLet  (NonEmpty Def) Term
  | TCase Term [(Pat, Term)]
  
  | TDup  Text

  -- Annotations
  | TLoc   Loc Term
  | TParen Term
  | TWild
  deriving(Show)


type Type = Term


-- -----------------------------------------------------------------------------
-- | Patterns

data Pat
  = PVar NameHint Text
  | PWild
  | PVal Value
  | PCon Text [(Plicity, Pat)]
  | PAnno Pat Type
  | PView Term Pat
  | PLoc Loc Pat
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Value

data Value
  = VInt Integer
  | VFloat Double
  | VChar Char
  | VBool Bool
  deriving (Show, Read, Eq, Ord)


-- -----------------------------------------------------------------------------
-- | Helpers

{-
pis :: [Pat (Type v) v] -> Term v -> Term v
pis ps t = foldr TPi t ps
-}


-- -----------------------------------------------------------------------------
-- | Locatable Instances

instance {-# OVERLAPS #-} Locatable Term where
  locOf = \case
    TLoc l _  -> l
    -- Ignores parens and others like lambda
    _     -> error "No location found"


instance Locatable Pat where
  locOf = \case
    PLoc l _ -> l
    -- No other annotations compete with PLoc
    _ -> error "Pattern does not have a location."

-- -----------------------------------------------------------------------------
-- | Pretty Instances

instance Pretty TopLevelDef where
  pretty = undefined

instance Pretty Def where
  pretty = undefined

instance Pretty Sig where
  pretty (Sig n t) =
    pretty n
      <+> "::"
      <+> pretty t


instance Pretty DataDef where
    pretty (DataDef n vs cs) =
      undefined

instance Pretty ConstrDef where
    pretty (ConstrDef n t) =
      pretty n <+> ":" <+> pretty t


instance Pretty Fixity where
  pretty (Fixity f p ops) =
    vcat
      [ "Fixity Declaration"
      , indent 2 $ vcat
        [ "Fixity:" <+> pretty (pack $ show f)
        , "Precedence:" <+> pretty (pack $ show p)
        , "Operators:" <+> pretty ops
        ]
      ]


instance Pretty Foreign where
  pretty = \case
    ForeignImport ft fn hn ty ->
      vcat
        [ "Foreign Import:"
        , indent 2 $ vcat
            [ "Foreign Type:" <+> pretty ft
            , "Foreign Name:" <+> pretty fn
            , "Hawk Name:"    <+> pretty hn
            , "Type Sig:"     <+> pretty ty
            ]
        ]

    ForeignExport ft hn ->
      vcat
        [ "Foreign Export:"
        , indent 2 $ vcat
          [ "Foreign Type:" <+> pretty ft
          , "Hawk Name:"    <+> pretty hn
          ]
        ]

instance Pretty ForeignType where
  pretty ForeignC =
    "ForeignC"


instance Pretty Term where
    pretty = \case
      TVar n      -> pretty n
      TVal v      -> pretty v
      
      TCon n      -> pretty n
      TPrim i a b -> pretty i <+> pretty a <+> pretty b
      
      TApp e1 p e2  -> pretty e1 <+> pretty e2
      TLam p pat e    ->
          "\\" <+> pretty pat
            <+> vsep [ "."
                     , indent 2 (pretty e)
                     ]

      TPi p pat t    ->
          parens (pretty pat)
            <+> "->"
            <+> pretty t

      TLPi p pat t   ->
          parens (pretty pat)
            <+> "->"
            <+> pretty t
      
      TLet xs e ->
        vsep  [ "let"
              , indent 2 ( vsep $ pretty <$> NE.toList xs )
              , "in" <> indent 2 (pretty e)
              ]
      
      TCase e brs ->
        vsep  [ "case" <+> pretty e <+> "of"
              , indent 2 $ vsep
                  [ pretty p <+> "->" <+> pretty br
                    | (p, br) <- brs
                  ]
              ]

      TDup e ->
        "dup" <+> pretty e
      
      TLoc _ e  -> pretty e -- ignore location
      TParen t  -> parens $ pretty t
      TWild     -> "_"


instance Pretty Pat where
  pretty = \case
    PVar h n ->
      pretty n

    PWild->
      "_"
    
    PVal v ->
      pretty v

    PCon c pats ->
      pretty c <+> hsep [pretty p | (a, p) <- pats]

    PAnno p t ->
      pretty p
        <+> ":"
        <+> pretty t

    PView n p ->
      pretty n
        <> "@"
        <> pretty p

    PLoc _ p ->
      pretty p -- Usually best to hide locations


instance Pretty Value where
  pretty = \case
    VInt v ->
      pretty v
        
    VFloat v ->
      pretty v
    
    VChar c ->
      squotes $ pretty c
    
    VBool v ->
      pretty v