module Language.Hawk.Syntax.Literal where

import Data.Data
import Data.Typeable

import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
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