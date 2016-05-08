module Language.Hawk.Syntax.Literal where


data Literal
  = IntNum Integer
  | FloatNum Double
  | Chr Char
  | Str String
  | Boolean Bool
  deriving (Eq, Ord)
  
  
toString :: Literal -> String
toString literal =
  case literal of
    IntNum n -> show n
    FloatNum n -> show n
    Chr c -> show c
    Str s -> s
    Boolean bool -> show bool