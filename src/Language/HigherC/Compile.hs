module Language.HigherC.Compile where

import Prelude hiding (lex)


import Control.Monad.Except
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text.IO as T

import Language.HigherC.Parse (parseObject)
import Language.HigherC.Parse.Error
import Language.HigherC.Lex (lex)
import Language.HigherC.Lex.Error

import qualified Language.HigherC.Syntax.Concrete as C


import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)


readObject :: FilePath -> IO C.Object
readObject objFp = do
  objText <- T.readFile objFp
  mtoks <- runExceptT $ do
    let lexResult = withExcept PLexErr (lex objFp objText)
    mapExceptT (return . runIdentity) lexResult

  case mtoks of
    Left err -> do
      putDoc $ pretty err
      error "Could not complete lexical analysis"

    Right toks -> do
      putDoc (vsep $ pretty <$> toks)
      let obj = (parseObject toks) { C.objFile = Just objFp }
      putDoc $ pretty obj
      return obj
