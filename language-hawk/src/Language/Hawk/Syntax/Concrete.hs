{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
            , FlexibleContexts
            , OverloadedStrings
            , LambdaCase
            , TypeSynonymInstances
            , FlexibleInstances
  #-}
module Language.Hawk.Syntax.Concrete
  ( module Language.Hawk.Syntax.Concrete
  , module X
  )
  where

import Language.Hawk.Syntax.Location as X
import Language.Hawk.Syntax.Builtin  as X

import GHC.Generics
import Data.Typeable (Typeable)

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import Data.Maybe


import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

-- -----------------------------------------------------------------------------
-- | Names

data Name
  = Name { nameText :: Text
         , nameLoc  :: Loc
         }
  deriving (Show, Generic, Typeable)


data QName
  = QName
    { qnameText :: Text
    , qnamePath :: [Text]
    , qnameLoc :: Loc
    }
  deriving (Show, Generic, Typeable)


mkName :: L Text -> Name
mkName (L l n) =
  Name { nameText = n, nameLoc = l }


mkQName :: L Text -> QName
mkQName (L l n) = case T.splitOn "." n of
  []     -> error "Empty name encountered"
  (n:[]) -> QName { qnameText = n, qnamePath = [], qnameLoc = l }
  ns     -> QName { qnameText = last ns, qnamePath = init ns, qnameLoc = l} 

-- -----------------------------------------------------------------------------
-- | Toplevel Constructs

data Src = Src FilePath [TopLevel]


data TopLevel
  = TMod Name [TopLevel]
  | TImport Name
  | TFunc Func
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
-- | Function

data Func
  = Func FuncDecl Block
  deriving (Show)

data FuncDecl
  = FuncDecl Name Args (Maybe Type)
  deriving (Show)


data Args = Args [Arg]
  deriving (Show)

data Arg
  = Arg Name (Maybe Type)
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Statement

data Block = Block [Stmt]
  deriving (Show)

data Stmt
  = SCall Exp [Exp]
  | SDecl Exp (Maybe Type) (Maybe Exp)
  | SAssign Exp (Maybe Type) Exp
  | SReturn Exp
  | SLoc Loc Stmt
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Expression

data Exp
  = EVar Name
  | ECon Name
  | EVal Val
  | EInstr Instr Exp Exp
  | ECall Name [Exp]
  | EType Exp Type
  | ELoc Loc Exp
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Type and Kind

data Type
  = TVar Name
  | TCon Name
  | TApp Type Type
  | TArr Type Type
  | TKind Type Kind
  | TLoc Loc Type
  deriving (Show)

data Kind
  = KType
  | KArr Kind Kind
  | KLoc Loc Kind
  deriving (Show)


-- -----------------------------------------------------------------------------
-- | Locatable

instance Locatable Exp where
  locOf = \case
    -- Usually, we only want a top level location
    ELoc l _ -> l
    _        -> error "Location not found!"



-- -----------------------------------------------------------------------------
-- | Patterns

{-
data Pat
  = PVar Text
  | PVal Value
  | PAs Text Pat
  | PCon Text [Pat]
  | PRec [(Text, Pat)]

  | PType Pat TypeEInst

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


instance Pretty Name where
  pretty (Name n _) = pretty n

instance Pretty Src where
  pretty (Src fp tls) =
    vcat [ "filename:" <+> pretty fp
         , vcat $ fmap pretty tls
         ]

instance Pretty TopLevel where
  pretty = \case
    TMod n tls -> vcat [ "module" <+> pretty n <+> "{"
                       , indent 4 (vsep $ fmap pretty tls)
                       , "}"
                       ]

    TImport n -> pretty n
    TFunc f   -> pretty f



instance Pretty Func where
  pretty (Func (FuncDecl n args Nothing) body) =
    pretty n <+> pretty args <+> pretty body

  pretty (Func (FuncDecl n args (Just ty)) body) =
    pretty n <+> pretty args <+> ":" <+> pretty ty <+> pretty body


instance Pretty Args where
  pretty (Args args) =
    tupled (pretty <$> args)


instance Pretty Arg where
  pretty (Arg n Nothing) = pretty n
  pretty (Arg n (Just ty)) = pretty n <+> colon <+> pretty ty


instance Pretty Block where
  pretty (Block stmts) =
    vsep [ lbrace
         , indent 4 (vsep [ s <+> semi | s <- fmap pretty stmts])
         , rbrace
         ]


instance Pretty Stmt where
    pretty = \case
      SCall n args -> pretty n <+> tupled (pretty <$> args)

instance Pretty Exp where
    pretty = \case
      -- Terms
      EVar n      -> pretty n
      ECon n      -> pretty n
      EVal v      -> pretty v
      EInstr i e1 e2 -> pretty (show i) <+> pretty e1 <+> pretty e2
      ECall n es   -> pretty n <+> ( tupled $ pretty <$> es)
      EType ty e   -> pretty e <+> ":" <+> pretty ty
      ELoc _ e     -> pretty e -- ignore location


instance Pretty Type where
    pretty = \case
      TVar n -> pretty n

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


