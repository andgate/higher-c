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
  = Clause [Pat] Exp
  deriving (Show)

data Sig
  = Sig Text QType
  deriving (Show)



------------------------------------------------------------------------
-- Data Definition

data DataDef
  = DataDef Text [(Text, Type)] [ConstrDef]
  deriving (Show)


data ConstrDef
  = ConstrDef Text Type
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Class Definition

data ClassDef
  = ClassDef Text [(Text, Type)] [MethodDef]
  deriving (Show)

data MethodDef
  = MethodDef Loc Text Exp
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
-- | Expressions

data Exp
  = EVar  Text
  | EDup  Text
  | EVal  Value
  | EOp   Text

  | ECon  Text
  | EPrim PrimInstr Exp Exp
  
  | EApp  Exp Exp
  | ELam  Pat Exp

  | ELet  (NonEmpty Def) Exp
  | ECase Exp [(Loc, Pat, Exp)]

  -- Annotations
  | EType  Exp Type
  | ELoc   Loc Exp
  | EParen Exp
  | EWild
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Type

data Type
  = TVar Text
  | TCon Text
  | TApp Type Type
  | TArr Type Type
  | TLoli Type Type
  | TForall (NonEmpty Text) Type

  -- Annotations
  | TKind Type Kind
  | TLoc Loc Type
  | TParen Type
  | TWild
  deriving (Show)


data QType
  = QType [Assert] Type
  deriving Show

data Assert
  = IsIn Text [Type]
  deriving Show

-- -----------------------------------------------------------------------------
-- | Kind

data Kind
  = KStar
  | KArr Kind Kind
  | KLoc Loc Kind
  | KParen Kind
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Patterns

data Pat
  = PVar Text
  | PVal Value
  | PAs Text Pat
  | PCon Text [Pat]
  | PType Pat Type
  | PLoc Loc Pat
  | PParen Pat
  | PWild
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


instance Pretty Exp where
    pretty = \case
      EVar n      -> pretty n
      EVal v      -> pretty v
      EDup e      -> pretty e <> "!"
      EOp op      -> pretty op
      
      ECon n      -> pretty n
      EPrim i a b -> pretty i <+> pretty a <+> pretty b
      
      EApp e1 e2  -> pretty e1 <+> pretty e2
      ELam pat e    ->
          "\\" <+> pretty pat
            <+> vsep [ "."
                     , indent 2 (pretty e)
                     ]
      
      ELet xs e ->
        vsep  [ "let"
              , indent 2 ( vsep $ pretty <$> NE.toList xs )
              , "in" <> indent 2 (pretty e)
              ]
      
      ECase e brs ->
        vsep  [ "case" <+> pretty e <+> "of"
              , indent 2 $ vsep
                  [ pretty p <+> "->" <+> pretty br
                    | (_, p, br) <- brs
                  ]
              ]

      EType e t -> pretty e <+> ":" <+> pretty t
      ELoc _ e  -> pretty e -- ignore location
      EParen e  -> parens $ pretty e
      EWild     -> "_"


instance Pretty Type where
    pretty = \case
      TVar n      -> pretty n
      TCon n      -> pretty n
      
      TApp t1 t2  -> pretty t1 <+> pretty t2
      TArr t1 t2  -> pretty t1 <+> "->" <+> pretty t2
      TLoli t1 t2  -> pretty t1 <+> "-o" <+> pretty t2
      
      TForall xs t -> "forall" <+> hsep (pretty <$> NE.toList xs) <+> "." <+> pretty t

      TKind t k -> pretty t <+> ":" <+> pretty k
      TLoc _ t  -> pretty t -- ignore location
      TParen t  -> parens $ pretty t
      TWild     -> "_"


instance Pretty QType where
  pretty (QType as t) =
     tupled (pretty <$> as) <+> "=>" <+> pretty t

instance Pretty Assert where
  pretty (IsIn n tys) =
     pretty n <+> hsep (pretty <$> tys)


instance Pretty Kind where
    pretty = \case
      KStar       -> "*"
      KArr k1 k2  -> pretty k1 <+> "->" <+> pretty k2
      KLoc _ k    -> pretty k -- ignore location
      KParen k    -> parens $ pretty k


instance Pretty Pat where
  pretty = \case
    PVar x -> pretty x
    PVal v -> pretty v

    PAs n p ->
      pretty n <> "@" <> parens (pretty p)

    PCon c pats ->
      pretty c <+> hsep (pretty <$> pats)

    PType p t ->
      pretty p <+> ":" <+> pretty t


    PLoc _ p -> pretty p -- omit location
    PParen p  -> parens $ pretty p
    PWild -> "_"

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