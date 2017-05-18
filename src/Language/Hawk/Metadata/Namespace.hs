module Language.Hawk.Metadata.Namespace where

import Data.Graph.Inductive (Gr)
import Data.Text (Text)
import Language.Hawk.Compile.Monad

import qualified Data.Graph.Inductive.Graph         as Graph
import qualified Language.Hawk.Syntax.Item          as I
import qualified Language.Hawk.Metadata.Schema      as Db

type Module = Text
type Import = Text

type DepGr = Gr Module Import


build :: Compiler ()
build = return ()