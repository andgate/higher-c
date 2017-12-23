module Language.Hawk.Syntax.Source (module X, SourceImage) where
  
import Language.Hawk.Syntax as X
import Language.Hawk.Syntax.Term.Source as X
import Language.Hawk.Syntax.Pattern.Source as X

import Data.Text (Text)

type SourceImage = Image Term Text (Pat (Term Text) Text)