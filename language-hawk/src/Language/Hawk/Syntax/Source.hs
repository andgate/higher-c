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

data Module a =
  Module
    { modName :: Text
    , modVars :: [Text]
    , modContents :: a
    }
  deriving(Show)

type TopLevelModule = Module [TopLevelDef]

-- -----------------------------------------------------------------------------
-- | Top Level Definition

data TopLevelDef
  = TopLevelFnDef       Def
  | TopLevelSig         Sig
  | TopLevelAliasDef    AliasDef
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
  = DataDef Text [Text] [ConstrDef]
  deriving (Show)


data ConstrDef
  = ConstrDef Text [Type]
  | RecordDef Text [(Text, Type)]
  deriving (Show)

------------------------------------------------------------------------
-- Type Alias Definition

data AliasDef
  = AliasDef Text [Text] Type
  deriving Show

-- -----------------------------------------------------------------------------
-- | Class Definition

data ClassDef
  = ClassDef [Assert] Text [Type] [Sig]
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Instance Definition

data InstDef
  = InstDef [Assert] Text [Type] [Def]
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
  -- Terms
  = EVar  Text
  | EVal  Value
  | EOp   Text

  -- Magic
  | EConS Text
  | EConH Text
  | EPrim PrimInstr Exp Exp
  
  -- Evaluation
  | EApp  Exp Exp
  | ELam  Pat Exp

  -- Imperative, Impure stuff
  | ESeq Exp Exp
  | EBind Text Exp
  | ESet Text Exp
  | EFree [Text]

  -- Control Flow
  | ECase Exp [(Pat, Exp)]
  | EIf Exp Exp Exp
  | EDo [Exp]

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

  | TRow [(Text, Type)] (Maybe Text)

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
  | KRow Kind
  | KArr Kind Kind
  | KLoc Loc Kind
  | KParen Kind
  | KWild
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Patterns

data Pat
  = PVar Text
  | PVal Value
  | PAs Text Pat
  | PCon Text [Pat]
  | PRec [(Text, Text)]

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
  pretty = \case
    TopLevelFnDef       x -> pretty x
    TopLevelSig         x -> pretty x
    TopLevelAliasDef    x -> pretty x
    TopLevelDataDef     x -> pretty x
    TopLevelClassDef    x -> pretty x
    TopLevelInstDef     x -> pretty x
    TopLevelForeignDef  x -> pretty x
    TopLevelFixityDef   x -> pretty x

instance Pretty Def where
  pretty (Def n c) =
    pretty n <+> pretty c

instance Pretty Clause where
  pretty (Clause pats e) =
    hsep (pretty <$> pats) <+> "=" <+> pretty e

instance Pretty Sig where
  pretty (Sig n qt) =
    pretty n <+> ":" <+> pretty qt





instance Pretty DataDef where
    pretty (DataDef n vs cs) =
      pretty n <+> hsep (pretty <$> vs) <+> hang 0 (":=" <+> hsep (punctuate (line <> pipe) (pretty <$> cs)))

instance Pretty ConstrDef where
    pretty  = \case
      ConstrDef n ts ->
        pretty n <+> hsep (pretty <$> ts)

      RecordDef n fs ->
        pretty n <+> encloseSep lbrace rbrace comma
                      [ pretty n <+> ":" <+> pretty t | (n, t) <- fs ]


instance Pretty AliasDef where
  pretty (AliasDef n xs t) =
    pretty n <+> hsep (pretty <$> xs) <+> "=" <+> pretty t


instance Pretty ClassDef where
  pretty = \case
    ClassDef [] n vs ms ->
      "class" <+> pretty n <+> hsep (pretty <$> vs)
        <+> "where" <> indent 2 (vsep (pretty <$> ms))

    ClassDef as n vs ms ->
      "class" <+> tupled (pretty <$> as) <+> "=>"
        <+> pretty n <+> hsep (pretty <$> vs)
        <+> "where" <> indent 2 (vsep (pretty <$> ms))


instance Pretty InstDef where
  pretty = \case
    InstDef [] n vs ds ->
      pretty n <+> hsep (pretty <$> vs)
        <+> "where" <> indent 2 (vsep (pretty <$> ds))

    InstDef as n vs ds ->
      tupled (pretty <$> as) <+> "=>"
        <+> pretty n <+> hsep (pretty <$> vs)
        <+> "where" <> indent 2 (vsep (pretty <$> ds))

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
      EOp op      -> pretty op
      
      EConS n     -> pretty n
      EConH n     -> pretty n
      EPrim i a b -> pretty i <+> pretty a <+> pretty b
      
      EApp e1 e2  -> pretty e1 <+> pretty e2
      ELam pat e    ->
          "\\" <+> pretty pat
            <+> vsep [ "."
                     , indent 2 (pretty e)
                     ]

      ESeq a b -> pretty a <> ";" <+> pretty b
      EBind v e -> pretty v <+> "<-" <+> pretty e
      ESet v e -> pretty v <+> "=" <+> pretty e
      EFree vs -> "free" <+> hcat (pretty <$> vs)

      -- Control Flow
      ECase e brs -> "case" <+> pretty e
                            <+> vcat [pretty p <+> "->" <+> pretty e | (p, e) <- brs]

      EIf p a b -> "if" <+> pretty p
                        <+> "then" <+> pretty a
                        <+> "else" <+> pretty b 

      EDo es -> "do" <+> vcat [pretty e | e <- es]

      -- Annotations
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
  pretty = \case
    QType [] t -> pretty t
    QType as t ->
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