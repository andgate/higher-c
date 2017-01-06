module Language.Hawk.Parse.Object where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta


import Language.Hawk.Parse.Expression
import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Layout
import Language.Hawk.Parse.Binding
import Language.Hawk.Parse.Type
import qualified Language.Hawk.Syntax.Object as Obj
import qualified Language.Hawk.Syntax.Binding as B
import qualified Language.Hawk.Syntax.Type as Ty


declareObj :: MonadicParsing m => [B.Source] -> Maybe Ty.Source -> m Obj.Source
declareObj [] _ = error "declareObj in Object.hs received and empty set of bindings, and cannot determine a name." -- hopefully this will never bubble up
declareObj (b:_) t = locate $
  (objdefsym >> (Obj.Object b t <$> expr))

obj :: MonadicParsing m => m Obj.Source
obj = locate $
  Obj.Object <$> binding <*> typesig0 <*> (objdefsym >> expr)