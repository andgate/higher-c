module Language.Hawk.Metadata.DepFinder where


import Data.Text.Lazy (Text)
import Language.Hawk.Compile.Monad
import Language.Hawk.Metadata.Schema


depFind :: Text -> Compiler [Text]
depFind src = []