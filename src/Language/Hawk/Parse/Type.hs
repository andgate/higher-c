module Language.Hawk.Parse.Type where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import Language.Hawk.Parse.Helpers
import Language.Hawk.Parse.Layout
import Language.Hawk.Parse.Name
import qualified Language.Hawk.Syntax.Type as Type
import qualified Language.Hawk.Syntax.Name as Name
import qualified Language.Hawk.Report.Annotation as A
import qualified Language.Hawk.Report.Region as R


typesig :: MonadicParsing m => m Type.Source
typesig =
  hasType *> ws *> tipe
  
  
tipe :: MonadicParsing m => m Type.Source
tipe =
      try typeArr
  <|> btype

   
typeArr :: MonadicParsing m => m Type.Source
typeArr = withRegion fnArgs Type.arrow <?> "Function Type"
  where
   fnArgs = btype `sepBy2` (ws *> rightArrow <* ws)
  

btype :: MonadicParsing m => m Type.Source
btype =
  try btype' <|> atype
    
btype' :: MonadicParsing m => m Type.Source
btype' =
  locate $ Type.App <$> atype <*> some (ws *> atype <* ws) <?> "Type constructor"
  

-- Single Type
atype :: MonadicParsing m => m Type.Source
atype =
  try tyPrim <|> try tyTuple <|> tyCon


-- Primitive Type
tyPrim :: MonadicParsing m => m Type.Source
tyPrim =
  locate $ Type.Con <$> tyPrimName <?> "Primitive Type"

  
tyTuple :: MonadicParsing m => m Type.Source
tyTuple = withRegion tupleArgs Type.tuple <?> "Tuple Type"
  where
    tupleArgs = parens (commaSep2 atype)
    
tyCon :: MonadicParsing m => m Type.Source
tyCon = locate $ Type.Con <$> conName

tyPrimName :: MonadicParsing m => m Name.Source
tyPrimName =
      string "void"
  <|> string "i1"
  <|> string "i8"
  <|> string "i16"
  <|> string "i32"
  <|> string "i64"
  <|> string "i128"
  <|> string "f16"
  <|> string "f32"
  <|> string "f64"
  <|> string "f128"
  <?> "Name of a primitive type"