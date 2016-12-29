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
  optional $ try typesig

typesig :: MonadicParsing m => m Type.Source
typesig =
  hasType >> typ2


-- Single Type
typ0 :: MonadicParsing m => m Type.Source
typ0 =
      (try tyPrim <?> "Primitive Type")
  <|> (try tyTuple <?> "Type Tuple")
  <|> (tyCon <?> "Type Constructor")


typ1 :: MonadicParsing m => m Type.Source
typ1 = 
  Type.apply <$> typ0 <*> many typ0


typ2 :: MonadicParsing m => m Type.Source
typ2 = withRegion arrArgs Type.arrow
  where
    arrArgs = arrowSep1 typ1



-- Primitive Type
tyPrim :: MonadicParsing m => m Type.Source
tyPrim =
  locate . try $ Type.Con <$> tyPrimName

  
tyTuple :: MonadicParsing m => m Type.Source
tyTuple = withRegion tupleArgs Type.tuple
  where
    tupleArgs = parens (commaSep typ2)
    

tyCon :: MonadicParsing m => m Type.Source
tyCon = locate $ Type.Con <$> conName


tyPrimName :: MonadicParsing m => m Name.Source
tyPrimName =
      stringTok "I1"
  <|> stringTok "I8"
  <|> stringTok "I16"
  <|> stringTok "I32"
  <|> stringTok "I64"
  <|> stringTok "I128"
  <|> stringTok "F16"
  <|> stringTok "F32"
  <|> stringTok "F64"
  <|> stringTok "F128" 