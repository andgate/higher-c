{-# LANGUAGE DeriveDataTypeable
           , OverloadedStrings
  #-}
module Language.Hawk.Syntax.Type where

import Data.Aeson ((.=))
import Data.Binary
import Data.Data
import Data.Text (Text)
import Language.Hawk.Parse.Lexer.Token (Token)
import Text.PrettyPrint.ANSI.Leijen ((<+>))

import qualified Data.Aeson                   as Json
import qualified Language.Hawk.Syntax.Name    as N
import qualified Text.PrettyPrint.ANSI.Leijen as PP


type Source
  = [Token]

type Valid
  = [Token]

type Typed
  = Type N.Typed

data Type n
  = App (Type n) [Type n]
  | Con n
  deriving (Eq, Show, Ord, Data, Typeable)
-- Type variables will require at least monomorphization,
-- and won't be possible for a while.
--  | Var TVar a

type TVar = String

  
  

-- These are built-in type constructors. The definition of type does not
-- directly contain these, for the sake of generality.

-- Arr (Type n) (Type n)
-- "_Arrow"

-- Primitive types
-- A list of different types, such as "_I32" and "_F32"

-- Ptr (Type n)
-- "_Pointer"

-- Array (Type n)
-- "_Array"

-- Vector (Type n)
-- "_Vector"

-- Tuple [Type n]
-- "_Tuple"
  
{-
fn :: [Source] -> R.Region -> Source
fn types region =
  let
    name = 
      Name.Name ("_Arr" ++ show (length types)) region
  in
    App (Con name region) types
-}


typeCon :: Text -> [Typed] -> Typed
typeCon n [] = Con $ N.builtinQ n
typeCon n args = App (Con $ N.builtinQ n) args

apply :: Typed -> [Typed] -> Typed
apply con [] = con
apply con args = App con args

unit :: Typed
unit = typeCon "_#_Unit_#_" []

arrow :: [Typed] -> Typed
arrow (arg:[]) = arg
arrow args = variadic "_#Arr_#_" args

tuple :: [Typed] -> Typed
tuple (arg:[]) = arg
tuple args = variadic "_#_Tuple_#_" args

variadic :: Text -> [Typed] -> Typed
variadic n ts =
    App (Con $ N.builtinQ n) ts
    


instance (PP.Pretty n) => PP.Pretty (Type n) where
  pretty (App con args) =
    PP.text "Type App:"
    PP.<$>
    PP.indent 2
      ( PP.text "con:" <+> PP.pretty con
        PP.<$>
        PP.text "args:" PP.<$> PP.indent 2 (PP.pretty args)
      )
    

  pretty (Con name) =
    PP.text "Type Con" <+> PP.dquotes (PP.pretty name)
    


instance (Json.ToJSON n) => Json.ToJSON (Type n) where
  toJSON (App con args) =
    Json.object
      [ "value" .= Json.String "App"
      , "con"   .= con
      , "args"  .= args
      ]
      
  toJSON (Con name) =
    Json.object
      [ "value" .= Json.String "Con"
      , "name"  .= name
      ]


instance (Binary n) => Binary (Type n) where
  put tipe =
    case tipe of
      App t1 t2 ->
        putWord8 0 >> put t1 >> put t2
        
      Con name ->
        putWord8 1 >> put name
        
      --Var name ->
      --  putWord8 2 >> put name
        
  get =
    do  n <- getWord8
        case n of
          0 -> App <$> get <*> get
          1 -> Con <$> get
          --2 -> Var <$> get
          _ -> error "Error reading a valid type from serialized string"
          