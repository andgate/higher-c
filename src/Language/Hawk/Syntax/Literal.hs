{-# LANGUAGE DeriveDataTypeable #-}
module Language.Hawk.Syntax.Literal where

import Data.Binary
import Data.Data

import Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data Literal
  = IntNum Integer
  | FloatNum Double
  | Chr Char
  | Str String
  | Boolean Bool
  deriving (Show, Eq, Ord, Data, Typeable)
  
  
toString :: Literal -> String
toString literal =
  case literal of
    IntNum n -> show n
    FloatNum n -> show n
    Chr c -> show c
    Str s -> s
    Boolean bool -> show bool
    
    
instance PP.Pretty Literal where
  pretty literal =
    case literal of
      IntNum v ->
         PP.string "Literal Int:" <+> PP.string (show v)
         
      FloatNum v ->
        PP.string "Literal Float:" <+> PP.string (show v)
      
      Chr v ->
        PP.string "Literal Char:" <+> PP.string (show v)
      
      Str v ->
        PP.string "Literal String:" <+> PP.string (show v)
      
      Boolean v ->
        PP.string "Literal Bool:" <+> PP.string (show v)
        
        
instance Binary Literal where
  get = do
    n <- getWord8
    case n of
      1 -> IntNum <$> get
      2 -> FloatNum <$> get
      3 -> Chr <$> get
      4 -> Str <$> get
      5 -> Boolean <$> get
      _ -> error "unexpected input"

  put literal =
    case literal of
      IntNum v    -> putWord8 1 >> put v
      FloatNum v  -> putWord8 2 >> put v
      Chr v       -> putWord8 3 >> put v
      Str v       -> putWord8 4 >> put v
      Boolean v   -> putWord8 5 >> put v