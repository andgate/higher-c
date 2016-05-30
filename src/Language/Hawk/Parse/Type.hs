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
  hasType *> ws *> typ2


-- Single Type
typ0 :: MonadicParsing m => m Type.Source
typ0 =
      tyPrim
  <|> tyTuple
  <|> tyCon


typ1 :: MonadicParsing m => m Type.Source
typ1 = 
  Type.apply <$> typ0 <*> many (try (ws *> typ0)) <?> "Type constructor"


typ2 :: MonadicParsing m => m Type.Source
typ2 = withRegion arrArgs Type.arrow <?> "Type arrow"
  where
    arrArgs = arrowSep1 typ1



-- Primitive Type
tyPrim :: MonadicParsing m => m Type.Source
tyPrim =
  locate $ Type.Con <$> tyPrimName <?> "Primitive Type"

  
tyTuple :: MonadicParsing m => m Type.Source
tyTuple = withRegion tupleArgs Type.tuple <?> "Tuple Type"
  where
    tupleArgs = parens (commaSep1 typ2)
    

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