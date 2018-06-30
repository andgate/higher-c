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

data Module =
  Module
    { modName :: Text
    , modVars :: [Text]
    , modDeps :: [Text]
    , modDecls :: [Decl]
    }
  deriving(Show)


-- -----------------------------------------------------------------------------
-- | Top Level Definition

data Decl
  = ExpDecl       Exp
  | DefDecl       Def
  | SigDecl       Sig
{-
  | TopLevelAliasDef    AliasDef
  | TopLevelDataDef     DataDef
  | TopLevelClassDef    ClassDef
  | TopLevelInstDef     InstDef
  | TopLevelForeignDef  Foreign
  | TopLevelFixityDef   Fixity
-}
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Definitions

data Def
  = Def Text [Text] Exp
  deriving (Show)

data Sig
  = Sig Text Type
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

data Exp
  -- Terms
  = EVar  Text
  | EVal  Value
  | EPrim PrimInstr
  
  -- Evaluation
  | EApp  Exp Exp
  | ELam  Text Exp

  -- Annotations
  | EType  Exp Type
  | ELoc Loc Exp
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
  = TCon Text
  | TArr Type Type
  | TLoc Loc Type
  deriving (Show)

instance Locatable Type where
  locOf = \case
    -- Usually, we only want a top level location
    TLoc l _  -> l
    _         -> error "Location not found!"


-- -----------------------------------------------------------------------------
-- | Kind

data Kind
  = KStar
  | KLoc Loc Kind
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Patterns

{-
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
-}



{-
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
-}

-- -----------------------------------------------------------------------------
-- | Fixity Declarations

{-
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

-}

-- -----------------------------------------------------------------------------
-- | Foreign Declarations

{-
data Foreign
  = ForeignImport ForeignType (L Text) (L Text) Type
  | ForeignExport ForeignType (L Text)
  deriving (Show)


data ForeignType =
  ForeignC
  deriving (Show)
-}





-- -----------------------------------------------------------------------------
-- | Pretty Instances

instance Pretty Module where
  pretty (Module n xs deps decls) =
    vcat $  [ pretty n 
                <+>  hcat (pretty <$> xs)
                <+> ":"
                <+> hcat (punctuate comma (pretty <$> deps))
            ] ++ (pretty <$> decls)


instance Pretty Decl where
  pretty = \case
    ExpDecl         x -> pretty x
    DefDecl         x -> pretty x
    SigDecl         x -> pretty x
{-
    TopLevelAliasDef    x -> pretty x
    TopLevelDataDef     x -> pretty x
    TopLevelClassDef    x -> pretty x
    TopLevelInstDef     x -> pretty x
    TopLevelForeignDef  x -> pretty x
    TopLevelFixityDef   x -> pretty x
-}

instance Pretty Def where
  pretty (Def n xs body) =
    pretty n <+> hcat (pretty <$> xs) <+> "=" <+> pretty body

instance Pretty Sig where
  pretty (Sig n qt) =
    pretty n <+> ":" <+> pretty qt


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


instance Pretty Exp where
    pretty = \case
      -- Terms
      EVar n      -> pretty n
      EVal v      -> pretty v
      
      -- Evaluation
      EApp e1 e2  -> pretty e1 <+> pretty e2
      
      ELam pat e    ->
          "\\" <+> pretty pat
            <+> vsep [ "->"
                     , indent 2 (pretty e)
                     ]

      -- Annotations
      EType e t -> pretty e <+> ":" <+> pretty t
      ELoc _ e  -> pretty e -- ignore location


instance Pretty Type where
    pretty = \case
      TCon n      -> pretty n
      TArr t1 t2  -> pretty t1 <+> "->" <+> pretty t2
      TLoc _ t  -> pretty t


instance Pretty Kind where
    pretty = \case
      KStar       -> "*"
      KLoc _ k    -> pretty k

{-

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
-}

{-
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
-}


