module Language.Hawk.Syntax.Statement where


type Block n e a t = [Statement n e a t]


data Statement n e a t
  = Do (Block n e a t) a
  | Call e a
  | Let n t e a
  | Assign n t e a
  | Break a
  | Return e a
  | If [(e, Block n e a t)] (Block n e a t) a
  | While e (Block n t e a) a
  | DoWhile (Block n e a t) e a