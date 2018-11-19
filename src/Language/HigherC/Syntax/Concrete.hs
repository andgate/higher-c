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
import Data.Set (Set)


import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set
import qualified Data.Text          as T

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

data TopLevel
  = TopLevel [TopLevelStmt]

data TopLevelStmt
  = TModule Module
  | TImport Import
  | TDecl Decl
  | TFuncDefn FuncDefn
  | TCtor  CtorDefn
  | TDtor  DtorDefn
  | TTypeDefn
  | TClass
  | TImpl
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
-- | Module

data Module =
  Module Loc ModulePath ModuleBlock

data ModulePath =
  MPath [Name]

data ModuleBlock =
  MBlock [TopLevelStmt]

data Import =
  Import ModulePath

-- -----------------------------------------------------------------------------
-- | Declaration

data Decl
  = Decl1 Loc Name (Maybe Type) Exp
  | Decl2 Loc Initializer (Maybe Type)
  deriving (Show)

data Initializer =
  Init Name [Exp]


-- -----------------------------------------------------------------------------
-- | Function Definition

data FuncDefn
  = FuncDefn (Set FuncSpec) FuncDecl Block

data FuncSpec
  = InlineFunc
  | RecursiveFunc

data FuncDecl
  = FuncDecl Name Scheme Parameters (Maybe Type)

data Parameters = Parameters [Parameter]

data Parameter
  = Parameter Name (Maybe Type)


-- -----------------------------------------------------------------------------
-- | Constructor/Destructor Definitions

data CtorDefn =
  CtorDefn Name Scheme Parameters InitList Block

data InitList =
  InitList [Initializer]

data DtorDefn =
  DtorDefn Name Scheme Parameters Block


-- -----------------------------------------------------------------------------
-- | Statement

data Block = Block [Stmt]

data Stmt
  = SExp Loc (Maybe Exp)
  | SDecl Loc (Set SDeclSpec) Decl

  | SBlock Loc Block
  | SWith Loc Exp Stmt

  | SBreak Loc
  | SContinue Loc
  | SReturn Loc Exp

  | SIf Loc Exp Stmt (Maybe Stmt)

  | SWhile Loc Exp Stmt
  | SDoWhile Loc Stmt Exp

  | SFor Loc (Either (Maybe Exp) Decl) (Maybe Exp) (Maybe Exp) Stmt

-- | SCase -- Locked until patterns are figured out


data SDeclSpec
  = StaticDecl

-- -----------------------------------------------------------------------------
-- | Expression

data Exp
  = EVar Name
  | ECon Name
  | EVal Val
  | EOps [Term Exp]
  | EInstr Instr Exp Exp
  | ECall Exp [Exp]

  | EAssign Exp Exp
  | EArrayAccess Exp Exp
  | EPtrAccess Exp Name
  | EMember Exp Name

  | EAs Exp Type

  | EParens Exp
  | EType Exp Type
  | ELoc Loc Exp

data OperatorChain a
  = Chain (ChainLink a) OperatorChain

data ChainLink a
  = OperandLink a
  | OperatorLink Name

-- -----------------------------------------------------------------------------
-- | Type and Kind

data Type
  = TVar Name
  | TCon Name

  | TApp Type [Type]
  | TArr Type Type

  | TRef   Type
  | TRVal  Type
  | TConst Type
  | TMut   Type
  | TArr   Type
  | TArrS  Type Exp

  | TParens Type
  | TKind Type Kind
  | TLoc Loc Type


data Kind
  = KType
  | KArr Kind Kind
  | KLoc Loc  Kind


-- -----------------------------------------------------------------------------
-- | Type Scheme

data Scheme =
  Scheme [Pred]

data Pred
  = Forall Name
  | IsIn Name (NonEmpty Type)


instance Semigroup Scheme where
  (<>) (Scheme p1) (Scheme p2) = Scheme (p1 <> p2)

instance Monoid Scheme where
  mempty = Scheme mempty

-- -----------------------------------------------------------------------------
-- | Smart Constructors

mkVar :: L Text -> Exp
mkVar i@(L l n) = ELoc l (EVar (mkName i))

mkCon :: L Text -> Exp
mkCon i@(L l n) = ELoc l (ECon (mkName i))

mkVal :: L Val -> Exp
mkVal (L l v) = ELoc l (EVal v)

mkTVar :: L Text -> Type
mkTVar i@(L l n) = TLoc l (TVar (mkName i))

mkTCon :: L Text -> Type
mkTCon i@(L l n) = TLoc l (TCon (mkName i))


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


instance Pretty QName where
  pretty (QName n ns _) = hcat (punctuate "." (pretty <$> ns'))
    where ns' = reverse (n:ns)


instance Pretty TopLevel where
  pretty (TopLevel stmts) =
    vcat (pretty <$> stmts)

instance Pretty TopLevelStmt where
  pretty = \case
    TModule mpath mblk -> "module" <+> pretty mpath <+> pretty mblock
    TImport i -> pretty i

    TDecl decl -> pretty decl
    TFuncDefn fn   -> pretty fn

    TCtor ctor -> pretty ctor
    TDtor dtor -> pretty dtor

    _ -> error "Undefined top level statement encountered"


instance Pretty Import where
  pretty (Import mpath) =
    "import" <+> pretty mpath <> ";"

instance Pretty ModuleBlock where
  pretty (MBlock stmts) =
    vcat [ "{"
         , indent 4 (vsep $ fmap pretty stmts)
         , "}"
         ]

instance Pretty ModulePath where
  pretty (MPath ns) =
    concatWith (surround dot) [pretty n | n <- ns]

instance Pretty Decl where
  pretty = \case
    Decl1 l n Nothing   body -> "let" <+> pretty n <+> "=" <+> pretty body <> ";"
    Decl1 l n (Just ty) body -> "let" <+> pretty n <> ":" <+> pretty ty <+> "=" <+> pretty body <> ";"

    Decl2 l init Nothing   -> "let" <+> pretty init <> ";"
    Decl2 l init (Just ty) -> "let" <+> pretty init <> ":" <+> pretty ty ";"


instance Pretty Initializer where
  pretty (Init n []) = pretty n
  pretty (Init n es) =
    pretty n <> tupled [pretty e | e <- es]


instance Pretty FuncDefn where
  pretty (FuncDefn specs fdecl blk)
    | Set.null specs
      = vcat [ pretty fdecl
             , pretty blk
             ]

    | otherwise
      = let pretty_specs = hcat (pretty <$> (Set.toList specs))
        in vcat [ pretty_specs <+> pretty fdecl
                , pretty blk
                ]


instance Pretty FuncSpec where
  pretty = \case
    InlineFunc    -> "inline"
    RecursiveFunc -> "rec"


instance Pretty FuncDecl where
  pretty (FuncDecl n scheme params Nothing)
    = pretty n <> pretty scheme <> pretty params
  pretty (FuncDecl n scheme params (Just typ))
    = pretty n <> pretty scheme <> pretty params <> ":" <+> typ

instance Pretty Parameters where
  pretty (Parameter args) =
    tupled (pretty <$> args)

instance Pretty Parameter where
  pretty (Parameter n mtyp) =
    case mtyp of
      Nothing  -> pretty n
      Just typ -> pretty n <> ":" <+> pretty typ



instance Pretty CtorDefn where
  pretty (CtorDefn n scheme params inits blk)
    = vcat [ pretty n <> pretty scheme <> pretty params
           , indent 2 (pretty inits)
           , pretty blk
           ]

instance Pretty InitList where
  pretty (InitList inits) = ":" <+> hsep (punctuate "," (pretty <$> inits))



instance Pretty DtorDefn where
  pretty (DtorDefn n scheme params blk)
    = vcat [ "~" <> pretty n <> pretty scheme <> pretty params
           , pretty blk
           ]


instance Pretty Block where
  pretty (Block stmts) =
    vsep [ lbrace
         , indent 4 (vsep $ fmap pretty stmts)
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


instance Pretty Scheme where
  pretty = \case
    Scheme    [] -> mempty
    Scheme preds -> encloseSep "<" ">" "," (pretty <$> preds)

instance Pretty Pred where
  pretty = \case
    Forall n   -> pretty n
    IsIn n tys -> pretty n <+> hcat (pretty <$> NE.toList tys)

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


