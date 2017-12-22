module Language.Hawk.Syntax.Scoped (module X, ScopedImage) where

import Language.Hawk.Syntax as X
import Language.Hawk.Syntax.Term.Scoped as X
import Language.Hawk.Syntax.Pattern.Scoped as X

type ScopedImage = Image Term () (Pat (Term ()) Text)