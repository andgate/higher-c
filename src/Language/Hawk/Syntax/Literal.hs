module Language.Hawk.Syntax.Literal where


data Literal a
  = IntNum Integer a
  | FloatNum Double a
  | Chr Char a
  | Str String a
  | Boolean Bool a
  deriving (Eq, Ord)
  
  
toString :: Literal a -> String
toString literal =
  case literal of
    IntNum n _ -> show n
    FloatNum n _ -> show n
    Chr c _ -> show c
    Str s _ -> s
    Boolean bool _ -> show bool