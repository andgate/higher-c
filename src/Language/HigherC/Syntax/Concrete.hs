{-# LANGUAGE  DeriveGeneric
            , DeriveDataTypeable
            , FlexibleContexts
            , OverloadedStrings
            , LambdaCase
            , TypeSynonymInstances
            , FlexibleInstances
  #-}
module Language.HigherC.Syntax.Concrete
  ( module Language.HigherC.Syntax.Concrete
  , module X
  )
  where

import Language.HigherC.Syntax.Location as X
import Language.HigherC.Syntax.Builtin  as X

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import qualified Data.List.NonEmpty as NE

-- -----------------------------------------------------------------------------
-- | Names

data Name
  = Name { nameText :: Text
         , namePath :: Maybe ModulePath
         , nameLoc  :: Loc
         }


mkName :: L Text -> Name
mkName (L l n) =
  Name { nameText = n, namePath = Nothing, nameLoc = l }


mkQName :: ModulePath -> L Text -> Name
mkQName mp n
  = Name
    { nameText = unL n
    , namePath = Just mp
    , nameLoc = locOf mp <> locOf n
    }


-- -----------------------------------------------------------------------------
-- | Toplevel Constructs

data TopLevel
  = TopLevel [TopLevelStmt]

data TopLevelStmt
  = TModule Module
  | TImport Import
  | TDecl Decl
  | TFuncDefn FuncDefn
  | TFuncExtern FuncExtern
  | TCtor  CtorDefn
  | TDtor  DtorDefn
  | TTypeDefn TypeDefn
  | TAliasDefn AliasDefn
  | TClass ClassDefn
  | TInst InstDefn
  | TOpDecl OpDecl

-- -----------------------------------------------------------------------------
-- | Module

data Module =
  Module Loc ModulePath ModuleBlock

data ModulePath =
  MPath Loc (NonEmpty Name)

data ModuleBlock =
  MBlock Loc [TopLevelStmt]

data Import =
  Import Loc ModulePath

-- -----------------------------------------------------------------------------
-- | Declaration

data Decl
  = Decl1 Loc DeclHead (Maybe TypeSig)
  | Decl2 Loc DeclHead (Maybe TypeSig) Exp
  | Decl3 Loc DeclHead [Exp] (Maybe TypeSig)

data DeclHead
  = DeclHead Loc Name

data TypeSig
  = TypeSig Loc Type


-- -----------------------------------------------------------------------------
-- | Function Definition

data FuncDefn
  = FuncDefn Loc FuncDecl Block

data FuncDecl
  = FuncDecl Loc (Maybe FuncSpecs) Name (Maybe Scheme) Parameters (Maybe TypeSig)

data FuncExtern
  = FuncExtern Loc Name Parameters TypeSig

data FuncSpecs = FuncSpecs Loc (NonEmpty FuncSpec)

data FuncSpec
  = InlineFunc Loc
  | RecursiveFunc Loc

data Parameters = Parameters [Parameter]

data Parameter
  = Parameter Loc Name (Maybe TypeSig)


-- -----------------------------------------------------------------------------
-- | Constructor/Destructor Definitions

-- Constructor Definition
data CtorDefn =
  CtorDefn Loc CtorDecl Block

-- Constructor Declaration
data CtorDecl =
  CtorDecl Loc Name Parameters (Maybe Inits)

-- Destructor Definition
data DtorDefn =
  DtorDefn Loc DtorDecl Block

-- Destructor Declaration
data DtorDecl =
  DtorDecl Loc Name Parameters

-- Initializer List
data Inits =
  Inits Loc (NonEmpty Initializer)

-- Variable Initializer
data Initializer =
  Init Loc Name [Exp]


-- -----------------------------------------------------------------------------
-- | Statement

data Block = Block Loc [Stmt]

data Stmt
  = SNop Loc
  | SExp Loc Exp
  | SDecl Loc (Maybe SDeclSpecs) Decl

  | SBlock Loc Block
  | SWith Loc Exp Stmt

  | SBreak Loc
  | SContinue Loc
  | SReturn Loc (Maybe Exp)
  | SThrow Loc Exp

  | SIf Loc Exp Stmt (Maybe Stmt)

  | SWhile Loc Exp Stmt
  | SDoWhile Loc Stmt Exp

  | SFor Loc (Either (Maybe Exp) Decl) (Maybe Exp) (Maybe Exp) Stmt

  | SCase Loc Exp Alts
  | STryCatch Loc Try [Catch] (Maybe Finally)


data Try
  = Try Loc Stmt

data Catch
  = Catch Loc Exp Stmt

data Finally
  = Finally Loc Stmt

-- Statement Declaration Specifier
data SDeclSpecs = SDeclSpecs Loc (NonEmpty SDeclSpec)

data SDeclSpec
  = StaticDecl Loc


-- -----------------------------------------------------------------------------
-- | Patterns

data Pat
  = PVar Name
  | PVal Loc Val
  | PAs Loc Name Pat
  | PCon Loc Name [Pat]
  | PRec Loc Name [RecFieldPat]

  | PType Loc Pat Type

  | PParen Loc Pat
  | PWild Loc

data RecFieldPat
  = PRecField Loc Name Pat

data Alts = Alts Loc (NonEmpty Alt)

data Alt
  = Alt Loc Pat Stmt

-- -----------------------------------------------------------------------------
-- | Expression

data Exp
  = EVar Name
  | ECon Name
  | EVal Loc Val

  | EOp Loc [OpTerm]
  | EOpPrefix  Loc Name Exp
  | EOpPostfix Loc Exp Name
  | EOpInfix   Loc Exp Name Exp
  | EInstr Loc Instr Exp Exp

  | ECall Loc Exp Arguments
  | EAssign Loc Exp Exp
  | EMember Loc Exp Name
  | EPtrAccess Loc Exp Name
  | EArrayAccess Loc Exp Exp

  | ENew Loc Exp
  | EDelete Loc Exp

  | EParens Loc Exp
  | EAs Loc Exp Type
  | EType Loc Exp Type


data OpTerm
  = Operand Exp
  | Operator Name


data Arguments
  = Arguments Loc [Exp]


-- -----------------------------------------------------------------------------
-- | Type and Kind

data Type
  = TVar Name
  | TCon Name
  | TOp Loc [TyOpTerm]

  | TApp Loc Type TypeArguments
  | TArr Loc Type Type

  | TRef   Loc Type
  | TRVal  Loc Type
  | TPtr   Loc Type
  | TConst Loc Type
  | TArray Loc Type
  | TArraySized Loc Type (L Integer)

  | TParens Loc Type
  | TKind Loc Type KindSig
  | TLoc Loc Type


data Kind
  = KType Loc
  | KArr Loc Kind Kind

data KindSig
  = KindSig Loc Kind

data TyOpTerm
  = TyOperator Name
  | TyOperand Type


data TypeArguments
  = TypeArguments Loc [Type]


data TypeParameters
  = TypeParameters Loc [TypeParameter]

data TypeParameter
  = TypeParameter Loc Name (Maybe KindSig)


-- -----------------------------------------------------------------------------
-- | Type Scheme and Predicates

data Scheme =
  Scheme Loc [Pred]

data Pred
  = Forall Loc Name
  | IsIn Loc Name (NonEmpty Type)


-- This helps a little bit
instance Semigroup Scheme where
  (<>) (Scheme l1 p1) (Scheme l2 p2) = Scheme (l1 <> l2) (p1 <> p2)

instance Monoid Scheme where
  mempty = Scheme mempty mempty


------------------------------------------------------------------------
-- Type Definition

data TypeDefn
  = TypeDefn Loc TypeDecl TyDefnBody

data TypeDecl
  = TypeDecl Loc Name (Maybe Scheme) TypeParameters


data TyDefnBody
  = TyDefnBody Loc [DataDefn]

data DataDefn
  = DataDefn Loc Name DataFields
  | ObjectDefn Loc Name ObjectFields

data DataFields
  = DataFields Loc [DataField]

data DataField
  = DataField Loc Type (Maybe DataFieldDefault)

data DataFieldDefault
  = DataFieldDefault Loc Exp

data ObjectFields
  = ObjectFields Loc [ObjectField]

data ObjectField
  = ObjectField Loc Name TypeSig (Maybe DataFieldDefault)


-- Type Alias Definition

data AliasDefn
  = AliasDefn Loc AliasDecl Type

data AliasDecl
  = AliasDecl Loc Name (Maybe Scheme) TypeParameters


-- -----------------------------------------------------------------------------
-- | Class Definition

data ClassDefn
  = ClassDefn Loc ClassDecl ClassBody

data ClassDecl
  = ClassDecl Loc Name (Maybe Scheme) TypeParameters

data ClassBody
  = ClassBody Loc [ClassMethod]

data ClassMethod
  = ClassMethod Loc FuncDecl (Maybe Block)

-- -----------------------------------------------------------------------------
-- | Instance Definition

data InstDefn
  = InstDefn Loc InstDecl InstBody

data InstDecl
  = InstDecl Loc Name (Maybe Scheme) TypeArguments

data InstBody
  = InstBody Loc [InstMethod]

data InstMethod
  = InstMethod Loc FuncDefn


-- -----------------------------------------------------------------------------
-- | Operator Declaration

-- A fixity declaration
data OpDecl
  = OpDecl Loc Fixity (L Integer) [Name]

-- The kinds of fixity
data Fixity
  = InfixN Loc
  | InfixL Loc
  | InfixR Loc
  | Prefix Loc
  | Postfix Loc


-- -----------------------------------------------------------------------------
-- | Smart Constructors

mkVal :: L Val -> Exp
mkVal (L l v) = EVal l v


-- -----------------------------------------------------------------------------
-- | Locatable


-- Names
instance Locatable Name where
  locOf (Name _ mp l) = maybe l locOf mp <> l


-- Toplevel
instance Locatable TopLevel where
  locOf (TopLevel stmts) = locOf stmts

instance Locatable TopLevelStmt where
  locOf = \case
    TModule m   -> locOf m
    TImport i   -> locOf i
    TDecl d     -> locOf d
    TFuncDefn f -> locOf f
    TCtor  c    -> locOf c
    TDtor  d    -> locOf d


-- Modules
instance Locatable Module where
  locOf (Module l _ _) = l

instance Locatable ModulePath where
  locOf (MPath l _) = l

instance Locatable ModuleBlock where
  locOf (MBlock l _) = l

instance Locatable Import where
  locOf (Import l _) = l


-- Declarations
instance Locatable Decl where
  locOf = \case
    Decl1 l _ _   -> l
    Decl2 l _ _ _ -> l
    Decl3 l _ _ _ -> l

instance Locatable DeclHead where
  locOf (DeclHead l _) = l

instance Locatable TypeSig where
  locOf (TypeSig l _) = l


-- Functions
instance Locatable FuncDefn where
  locOf (FuncDefn l _ _) = l

instance Locatable FuncDecl where
  locOf (FuncDecl l _ _ _ _ _) = l

instance Locatable FuncSpecs where
  locOf (FuncSpecs l _) = l

instance Locatable FuncSpec where
  locOf = \case
    InlineFunc    l -> l
    RecursiveFunc l -> l

instance Locatable Parameters where
  locOf (Parameters ps) = mconcat (map locOf ps)

instance Locatable Parameter where
  locOf (Parameter l _ _) = l


-- Constructors and destructors
instance Locatable CtorDefn where
  locOf (CtorDefn l _ _) = l

instance Locatable CtorDecl where
  locOf (CtorDecl l _ _ _) = l

instance Locatable DtorDefn where
  locOf (DtorDefn l _ _) = l

instance Locatable DtorDecl where
  locOf (DtorDecl l _ _) = l

instance Locatable Inits where
  locOf (Inits l _) = l

instance Locatable Initializer where
  locOf (Init l _ _) = l

-- Statements
instance Locatable Block where
  locOf (Block l _) = l

instance Locatable Stmt where
  locOf = \case
    SNop l -> l
    SExp l _ -> l
    SDecl l _ _ -> l

    SBlock l _ -> l
    SWith l _ _ -> l

    SBreak l -> l
    SContinue l -> l
    SReturn l _ -> l

    SIf l _ _ _ -> l

    SWhile l _ _ -> l
    SDoWhile l _ _ -> l

    SFor l _ _ _ _ -> l
    SCase l _ _ -> l
    STryCatch l _ _ _ -> l


instance Locatable Try where
  locOf (Try l _) = l

instance Locatable Catch where
  locOf (Catch l _ _) = l

instance Locatable Finally where
  locOf (Finally l _) = l


instance Locatable SDeclSpecs where
  locOf (SDeclSpecs l _) = l

instance Locatable SDeclSpec where
  locOf = \case
    StaticDecl l -> l


-- Patterns
instance Locatable Pat where
  locOf = \case
    PVar n -> locOf n
    PVal l _ -> l
    PAs l _ _ -> l
    PCon l _ _ -> l
    PRec l _ _ -> l
    PType l _ _ -> l
    PParen l _ -> l
    PWild l -> l


instance Locatable RecFieldPat where
  locOf (PRecField l _ _) = l


instance Locatable Alts where
  locOf (Alts l _) = l

instance Locatable Alt where
  locOf (Alt l _ _) = l


-- Expressions
instance Locatable Exp where
  locOf = \case
    EVar n -> locOf n

instance Locatable OpTerm where
  locOf = \case
    Operator n -> locOf n
    Operand e -> locOf e

instance Locatable Arguments where
  locOf (Arguments l _) = l


-- Types
instance Locatable Type where
  locOf = \case
    TVar n -> locOf n

instance Locatable Kind where
  locOf = \case
    KType l    -> l
    KArr l _ _ -> l

instance Locatable KindSig where
  locOf (KindSig l _) = l

instance Locatable TyOpTerm where
  locOf = \case
    TyOperator n -> locOf n
    TyOperand  ty -> locOf ty

instance Locatable TypeArguments where
  locOf (TypeArguments l _) = l


instance Locatable TypeParameters where
  locOf (TypeParameters l _) = l

instance Locatable TypeParameter where
  locOf (TypeParameter l _ _) = l


-- Type schems and predicates
instance Locatable Scheme where
  locOf (Scheme l _) = l

instance Locatable Pred where
  locOf = \case
    Forall l _ -> l
    IsIn l _ _  -> l


-- Type Definitions
instance Locatable TypeDefn where
  locOf (TypeDefn l _ _) = l

instance Locatable TypeDecl where
  locOf (TypeDecl l _ _ _) = l

instance Locatable TyDefnBody where
  locOf (TyDefnBody l _) = l

instance Locatable DataDefn where
  locOf (DataDefn l _ _) = l
  locOf (ObjectDefn l _ _) = l

instance Locatable DataFields where
  locOf (DataFields l _) = l

instance Locatable DataField where
  locOf (DataField l _ _) = l

instance Locatable DataFieldDefault where
  locOf (DataFieldDefault l _) = l

instance Locatable ObjectFields where
  locOf (ObjectFields l _) = l

instance Locatable ObjectField where
  locOf (ObjectField l _ _ _) = l


-- Type alias
instance Locatable AliasDefn where
  locOf (AliasDefn l _ _) = l

instance Locatable AliasDecl where
  locOf (AliasDecl l _ _ _) = l


-- Class Definitions
instance Locatable ClassDefn where
  locOf (ClassDefn l _ _) = l

instance Locatable ClassDecl where
  locOf (ClassDecl l _ _ _) = l

instance Locatable ClassBody where
  locOf (ClassBody l _) = l

instance Locatable ClassMethod where
  locOf (ClassMethod l _ _) = l


-- Instance Definitions
instance Locatable InstDefn where
  locOf (InstDefn l _ _) = l

instance Locatable InstDecl where
  locOf (InstDecl l _ _ _) = l

instance Locatable InstBody where
  locOf (InstBody l _) = l

instance Locatable InstMethod where
  locOf (InstMethod l _) = l


-- Operator Declaration
instance Locatable OpDecl where
  locOf (OpDecl l _ _ _) = l

instance Locatable Fixity where
  locOf = \case
    InfixN  l -> l
    InfixR  l -> l
    InfixL  l -> l
    Postfix l -> l
    Prefix  l -> l

-- -----------------------------------------------------------------------------
-- | Pretty Instances


-- Names
instance Pretty Name where
  pretty (Name n mp _) = case mp of
    Nothing -> pretty n
    Just p -> pretty p <> "::" <> pretty n
    

-- Toplevel
instance Pretty TopLevel where
  pretty (TopLevel stmts) =
    vcat (pretty <$> stmts)

instance Pretty TopLevelStmt where
  pretty = \case
    TModule m -> pretty m
    TImport i -> pretty i

    TDecl decl -> pretty decl
    TFuncDefn fn   -> pretty fn

    TCtor ctor -> pretty ctor
    TDtor dtor -> pretty dtor

    _ -> error "Undefined top level statement encountered"


-- Modules
instance Pretty Module where
  pretty (Module _ mpath mblk) = "module" <+> pretty mpath <+> pretty mblk

instance Pretty Import where
  pretty (Import _ mpath) =
    "import" <+> pretty mpath <> ";"


instance Pretty ModuleBlock where
  pretty (MBlock _ stmts) =
    vcat [ "{"
         , indent 4 (vsep $ fmap pretty stmts)
         , "}"
         ]

instance Pretty ModulePath where
  pretty (MPath _ ns) =
    concatWith (surround dot) [pretty n | n <- NE.toList ns]


-- Declarations
instance Pretty Decl where
  pretty = \case
    Decl1 _ h msig        -> pretty h <> pretty msig <> ";"
    Decl2 _ h msig body   -> pretty h <> pretty msig <+> "=" <+> pretty body <> ";"
    Decl3 _ h inits msig  -> pretty h <> tupled (pretty <$> inits) <> pretty msig <> ";"

instance Pretty DeclHead where
  pretty (DeclHead _ n) = "let" <> pretty n

instance Pretty TypeSig where
  pretty (TypeSig _ ty) = ":" <+> pretty ty


-- Functions
instance Pretty FuncDefn where
  pretty (FuncDefn _ fdecl blk) =
    pretty fdecl <+> pretty blk


instance Pretty FuncDecl where
  pretty (FuncDecl _ n specs scheme params ts) =
    pretty n <> pretty specs <> pretty scheme <> pretty params <> pretty ts


instance Pretty FuncSpecs where
  pretty (FuncSpecs l s) = hsep . fmap pretty $ NE.toList s

instance Pretty FuncSpec where
  pretty = \case
    InlineFunc _    -> "inline"
    RecursiveFunc _ -> "rec"

instance Pretty Parameters where
  pretty (Parameters args) =
    tupled (pretty <$> args)

instance Pretty Parameter where
  pretty (Parameter _ n mtysig) =
    pretty n <> ":" <+> pretty mtysig


-- Constructor definitions
instance Pretty CtorDefn where
  pretty (CtorDefn _ decl blk)
    = vsep [ pretty decl
           , pretty blk
           ]

instance Pretty CtorDecl where
  pretty (CtorDecl _ n params inits) =
    vsep [ pretty n <> pretty params
         , indent 2 (pretty inits)
         ]

instance Pretty Inits where
  pretty (Inits _ inits) = ":" <+> vsep (punctuate "," (pretty <$> NE.toList inits))


instance Pretty Initializer where
  pretty (Init _ n es) =
    pretty n <> tupled (pretty <$> es)


instance Pretty DtorDefn where
  pretty (DtorDefn _ decl blk) =
    vsep [ pretty decl
         , pretty blk
         ]

instance Pretty DtorDecl where
  pretty (DtorDecl _ n params) =
     "~" <> pretty n <> pretty params

instance Pretty Block where
  pretty (Block _ stmts) =
    vsep [ lbrace
         , indent 4 (vsep $ fmap pretty stmts)
         , rbrace
         ]


instance Pretty Stmt where
    pretty = \case
      SNop _ -> ";"
      SExp _ e -> pretty e <> ";"
      SDecl _ specs decl ->
        pretty specs <+> pretty decl
    
      SBlock _ blk -> pretty blk
      SWith _ e body ->
        "with (" <> pretty e <> ")" <+> pretty body
    
      SBreak _ -> "break;"
      SContinue _ -> "continue;"
      SReturn _ e -> "return" <+> pretty e <> ";"
    
      SIf _ p then_branch Nothing ->
        "if (" <> pretty p <> ")" <+> pretty then_branch

      SIf _ p then_branch (Just else_branch) ->
        vcat [ "if (" <> pretty p <> ")" <+> pretty then_branch
             , "else" <+> pretty then_branch
             ]
        
      SWhile _ p body ->

        vcat [ "do" <+> pretty body
             , "while (" <> pretty p <> ");"
             ]

      SDoWhile _ body p ->
        "while (" <> pretty p <> ")" <+> pretty body

      SFor _ header cond counter body ->
        let pheader = case header of
                        Left (Nothing) -> ";"
                        Left (Just e) -> pretty e <> ";"
                        Right decl -> pretty decl

            pcond   = case cond of
                        Nothing -> ";"
                        Just e  -> pretty e <> ";"

            pcounter = case counter of
                         Nothing -> mempty
                         Just e -> pretty e

        in "for (" <> pheader <> pcond <> pcounter <> ")" <+> pretty body

      SCase _ e alts ->
        vsep [ "case (" <> pretty e <> ") {"
             , pretty alts
             , "}"
             ]

      STryCatch _ t cs mf ->
        vsep [ pretty t
             , vsep (pretty <$> cs)
             , pretty mf
             ]

instance Pretty Try where
  pretty (Try _ stmt) =
    "try" <+> pretty stmt

instance Pretty Catch where
  pretty (Catch _ ex stmt) =
    "catch (" <> pretty ex <> ")" <+> pretty stmt

instance Pretty Finally where
  pretty (Finally _ stmt) =
    "finally" <+> pretty stmt


instance Pretty SDeclSpecs where
  pretty (SDeclSpecs _ specs) =
    hsep (pretty <$> NE.toList specs)

instance Pretty SDeclSpec where
  pretty = \case
    StaticDecl _ -> "static"



instance Pretty Pat where
  pretty = \case
    PVar n -> pretty n


instance Pretty Alts where
  pretty (Alts _ alts) =
    vsep (pretty <$> NE.toList alts)

instance Pretty Alt where
  pretty (Alt _ p stmt) =
    pretty p <+> pretty stmt


instance Pretty Exp where
    pretty = \case
      -- Terms
      EVar n      -> pretty n
      ECon n      -> pretty n
      EVal _ v      -> pretty v
      EInstr _ i e1 e2 -> pretty (show i) <+> pretty e1 <+> pretty e2
      ECall _ n args -> pretty n <> pretty args
      EType _ ty e   -> pretty e <+> ":" <+> pretty ty


instance Pretty Arguments where
  pretty (Arguments _ es) = tupled $ pretty <$> es


instance Pretty Type where
    pretty = \case
      TVar n -> pretty n

instance Pretty Kind where
    pretty = \case
      KType _    -> "Type"
      KArr _ a b -> pretty a <+> "->" <+> pretty b

instance Pretty KindSig where
  pretty (KindSig _ k) = ":" <+> pretty k


instance Pretty Scheme where
  pretty = \case
    Scheme _    [] -> mempty
    Scheme _ preds -> encloseSep "<" ">" "," (pretty <$> preds)

instance Pretty Pred where
  pretty = \case
    Forall _ n   -> pretty n
    IsIn _ n tys -> pretty n <+> hcat (pretty <$> NE.toList tys)

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


