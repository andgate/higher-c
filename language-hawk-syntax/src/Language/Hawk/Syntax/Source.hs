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

data Module i =
  Module
    { modName :: Text
    , modVars :: [Text]
    , modDeps :: [Text]
    , modItems :: [i]
    }
  deriving(Show)

type SrcModule = Module TopLevelDef

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
-- | Value

data Value
  = VInt Integer
  | VFloat Double
  | VChar Char
  | VBool Bool
  deriving (Show, Read, Eq, Ord)


-- -----------------------------------------------------------------------------
-- | Expressions


-- Memory model: Stack only vars
-- Evaluation model: Call-by-value

data Exp
  -- Terms
  = EVar  Text
  | ECon  Text
  | EVal  Value
  | EOp   Text

  | EPrimCon PrimCon
  | EPrimInstr PrimInstr
  
  -- Evaluation
  | EApp  Exp Exp
  | ELam  [Pat] Exp
  | ELet [LetStmt] Exp

  -- Data manipulation
  | ERec Exp [(Text, Exp)]
  | ECase Exp [(Loc, Pat, Exp)]
  | EIf Exp Exp Exp
  | EDo [Exp]

  -- Built-in Containers
  | ETuple [Exp]
  | EArray [Exp]

  -- Annotations
  | EType  Exp Type
  | ELoc   Loc Exp
  | EParen Exp
  | EWild
  deriving (Show)


-- Let statements allow variable
-- and internal function binding.
data LetStmt
  = LBind Pat Exp
  | LDef Text [Pat] Exp
  deriving (Show)


instance Locatable Exp where
  locOf = \case
    -- Usually, we only want a top level location
    ELoc l _ -> l
    _        -> error "Location not found!"

-- -----------------------------------------------------------------------------
-- | Type

data Type
  -- Terms
  = TVar Text
  | TCon Text
  | TOp Text
  
  -- Application
  | TApp Type Type
  | TArr Type Type
  | TForall [Text] Type

  -- Record Type
  | TRow [(Text, Type)] (Maybe Text)

  -- Simple containers
  | TTuple [Type]
  | TArray Type

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


instance Locatable Type where
  locOf = \case
    -- Usually, we only want a top level location
    TLoc l _  -> l
    _         -> error "Location not found!"


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
  | PRec [(Text, Pat)]

  | PType Pat Type
  | PLoc Loc Pat
  | PParen Pat
  | PWild
  deriving (Show)





-- -----------------------------------------------------------------------------
-- | Helpers

{-
pis :: [Pat (Type v) v] -> Term v -> Term v
pis ps t = foldr TPi t ps
-}


-- -----------------------------------------------------------------------------
-- | Pretty Instances

instance Pretty i => Pretty (Module i) where
  pretty (Module n xs deps cs) =
    vcat $  [ pretty n 
                <+>  hcat (pretty <$> xs)
                <+> ":"
                <+> hcat (punctuate comma (pretty <$> deps))
            ] ++ (pretty <$> cs)


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
      -- Terms
      EVar n      -> pretty n
      ECon n      -> pretty n
      EOp op      -> pretty op
      
      -- Evaluation
      EApp e1 e2  -> pretty e1 <+> pretty e2
      
      ELam pat e    ->
          "\\" <+> pretty pat
            <+> vsep [ "->"
                     , indent 2 (pretty e)
                     ]

      ELet xs e -> "let" <+> vcat (pretty <$> xs) <> "in" <+> pretty e


      -- Data manipulation
      ERec e brs -> encloseSep  (pretty e <+> lbrace)
                                rbrace
                                comma
                                [pretty n <+> pretty e | (n, e) <- brs]
      
      ECase e brs -> "case" <+> pretty e <+> "of"
                            <+> vcat [pretty p <+> "->" <+> pretty e | (_, p, e) <- brs]

      EIf p a b -> "if" <+> pretty p
                        <+> "then" <+> pretty a
                        <+> "else" <+> pretty b 

      EDo es -> "do" <+> vcat [pretty e | e <- es]


      -- Built-in Containers
      ETuple es -> tupled (pretty <$> es)
      EArray es -> list (pretty <$> es)


      -- Annotations
      EType e t -> pretty e <+> ":" <+> pretty t
      ELoc _ e  -> pretty e -- ignore location
      EParen e  -> parens $ pretty e
      EWild     -> "_"


instance Pretty LetStmt where
    pretty = \case
      LBind p e -> pretty p <+> "=" <+> pretty e
      LDef n xs e -> pretty n <+> hcat (pretty <$> xs) <+> "=" <+> pretty e


instance Pretty Type where
    pretty = \case
      -- Terms
      TVar n      -> pretty n
      TCon n      -> pretty n

      -- Application
      TApp t1 t2  -> pretty t1 <+> pretty t2
      TArr t1 t2  -> pretty t1 <+> "->" <+> pretty t2
      TForall xs t -> "forall" <+> hsep (pretty <$> xs) <+> "." <+> pretty t

      -- Record Type
      TRow recs mn -> encloseSep lbrace
                                 ("|" <+> pretty mn <+> rbrace)
                                 comma
                                 ([ pretty n <+> ":" <+> pretty t |(n, t) <- recs ])

      -- Simple containers
      TTuple ts -> tupled (pretty <$> ts)
      TArray t -> list [pretty t]

      -- Annotations
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
