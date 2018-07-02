{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
            , FlexibleContexts
            , OverloadedStrings
            , LambdaCase
            , TypeSynonymInstances
            , FlexibleInstances
  #-}
module Language.Hawk.Syntax.Source
  ( module Language.Hawk.Syntax.Source
  , module X
  )
  where

import GHC.Generics
import Data.Typeable (Typeable)

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
  = TermDecl Term
  | FunDecl  Fun
  | SigDecl  Sig
  deriving (Show)

{-
  | TopLevelAliasDef    AliasDef
  | TopLevelDataDef     DataDef
  | TopLevelClassDef    ClassDef
  | TopLevelInstDef     InstDef
  | TopLevelForeignDef  Foreign
  | TopLevelFixityDef   Fixity
-}


-- -----------------------------------------------------------------------------
-- | Definitions

data Fun
  = Fun Text [Text] Term
  deriving (Show)

data Sig
  = Sig Text Type
  deriving (Show)

-- -----------------------------------------------------------------------------
-- | Terms

type Type = Term

data Plicity = Implicit | Explicit
  deriving (Show, Generic, Typeable)

data Term
  = Type  -- Types are first class
  | Linear -- Types can be normal or linear
  
  -- x
  | TVar  Text 
  -- A
  | TCon  Text
  -- integers, floats, characters, booleans, etc.
  | TVal  PrimVal
  
  -- #add, #sub, #eq, etc.
  | TPrim PrimInstr Term Term
  
  -- f a
  | TApp   Term (NonEmpty Term)
  
  -- Lambda types represent term functions
  -- λ x . body
  -- \ x . body
  -- forall x . body
  | TLam (NonEmpty Pat) Term

  -- Pi types represent type functions
  -- Π (x1:A1) .. (xn:An) -> B x1 .. xn
  -- \ (x1:A1) .. (xn:An) -> B x1 .. xn
  -- \ x -> B x
  -- A -> B
  --
  -- We can also have an implicit Pi
  -- \@(x1) .. @(xn:An) -> B
  -- \@(x1) .. @(xn) -> B
  -- @(x:A) B -> C
  -- @x A
  | TPi (NonEmpty PlicitPat) Type

  -- let x = t in body
  | TLet (NonEmpty PatBind) Term

  -- Dependent product, Sigma Type, or a Tuple
  -- We can accept multiple patterns.
  -- (x, y)
  -- (A, B)
  -- (x:A, B)
  -- We can have multiple dependent terms
  -- (x:A, y:B x, C x y) : \ x:A (y:B) -> (x:A, y:B x, C x y)
  -- Or a single term without dependence
  -- (x)
  | TSigma [Pat] Term

  -- Annotations
  -- x : A
  | TAnn  Term Type
  
  -- Location decorator
  | TLoc Loc Term
  deriving (Show, Generic, Typeable)


data Val
  = VPrim PrimVal
  | VLam Pat Term
  | VPi Pat Plicity Term
  deriving (Show, Generic, Typeable)


data Pat
  = PVar Text
  | PWild
  | PAnn Pat Term
  | PParen Pat
  | PLoc Loc Pat
  deriving (Show, Generic, Typeable)


data PlicitPat = PlicitPat Loc Plicity Pat
  deriving (Show, Generic, Typeable)

data PatBind = PatBind Pat Term
  deriving (Show, Generic, Typeable)


instance Locatable Term where
  locOf = \case
    -- Usually, we only want a top level location
    TLoc l _ -> l
    _        -> error "Location not found!"


checkPatAnn :: Pat -> Maybe Term
checkPatAnn = \case 
  PVar _   -> Nothing
  PWild    -> Nothing
  PAnn _ t -> Just t
  PLoc _ p -> checkPatAnn p
  PParen p -> checkPatAnn p


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
    TermDecl        x -> pretty x
    FunDecl         x -> pretty x
    SigDecl         x -> pretty x
{-
    TopLevelAliasDef    x -> pretty x
    TopLevelDataDef     x -> pretty x
    TopLevelClassDef    x -> pretty x
    TopLevelInstDef     x -> pretty x
    TopLevelForeignDef  x -> pretty x
    TopLevelFixityDef   x -> pretty x
-}

instance Pretty Fun where
  pretty (Fun n xs body) =
    pretty n <+> hcat (pretty <$> xs) <+> "=" <+> pretty body

instance Pretty Sig where
  pretty (Sig n qt) =
    pretty n <+> ":" <+> pretty qt


instance Pretty Term where
    pretty = \case
      -- Terms
      Type        -> "Type"
      Linear      -> "Linear"

      TVar n      -> pretty n
      TCon n      -> pretty n
      TVal v      -> pretty v
      
      TPrim i t t' -> pretty (show i) <+> pretty t <+> pretty t'
      
      TApp e1 e2  -> pretty e1 <+> hcat (pretty <$> NE.toList e2)
      
      TLam ps body -> "\\" <+> hcat (pretty <$> NE.toList ps) <+> "." <+> pretty body 

      TPi ps body -> "\\" <+> hcat (pretty <$> NE.toList ps) <+> "->" <+> pretty body

      TLet vs body -> "let" <+> nest 2 (vcat (pretty <$> NE.toList vs)) <+> pretty body
      TSigma ps t -> tupled ((pretty <$> ps) ++ [pretty t])
      
      TAnn t t' -> pretty t <+> ":" <+> pretty t'
      TLoc _ t  -> pretty t -- ignore location


instance Pretty Pat where
  pretty = \case
    PVar x -> pretty x
    PWild -> "_"
    PAnn p t -> pretty p <+> ":" <+> pretty t
    PParen p  -> parens $ pretty p
    PLoc _ p -> pretty p -- omit location

instance Pretty PlicitPat where
  pretty (PlicitPat _ Implicit p) = "@" <> pretty p
  pretty (PlicitPat _ Explicit p) = pretty p

instance Pretty PatBind where
  pretty (PatBind p t) = pretty p <+> "=" <+> pretty t

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


