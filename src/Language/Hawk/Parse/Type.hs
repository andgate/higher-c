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


typesig0 :: MonadicParsing m => m (Maybe Type.Source)
typesig0 =
  optional (try typesig)

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
  Type.apply <$> typ0 <*> many (try (ws *> typ0))


typ2 :: MonadicParsing m => m Type.Source
typ2 = withRegion arrArgs Type.arrow
  where
    arrArgs = arrowSep1 typ1



-- Primitive Type
tyPrim :: MonadicParsing m => m Type.Source
tyPrim =
  locate $ Type.Con <$> tyPrimName

  
tyTuple :: MonadicParsing m => m Type.Source
tyTuple = withRegion tupleArgs Type.tuple
  where
    tupleArgs = parens (commaSep1 typ2)
    

tyCon :: MonadicParsing m => m Type.Source
tyCon = locate $ Type.Con <$> conName


tyPrimName :: MonadicParsing m => m Name.Source
tyPrimName =
      string "()"
  <|> string "I1"
  <|> string "I8"
  <|> string "I16"
  <|> string "I32"
  <|> string "I64"
  <|> string "I128"
  <|> string "F16"
  <|> string "F32"
  <|> string "F64"
  <|> string "F128" 