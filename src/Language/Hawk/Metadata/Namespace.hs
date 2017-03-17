module Language.Hawk.Metadata.Namespace where

import Data.Graph.Inductive (Gr)
import Data.Text.Lazy (Text)
import Language.Hawk.Compile.Monad

import qualified Data.Graph.Inductive.Graph         as Graph
import qualified Language.Hawk.Syntax.IBox          as IB
import qualified Language.Hawk.Metadata.Schema      as Db

type Module = (Text, [Text], IB.Ibox)
type Import = IB.IBox

type DepGr = Gr Module Import


build :: Compiler ()
build src = return []