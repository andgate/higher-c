{-# Language LambdaCase, OverloadedStrings #-}
module Language.Hawk.Value where

import Data.Text (Text)
import Language.Hawk.Syntax.Bound
import Language.Hawk.Syntax.Prim
import Data.Text.Prettyprint.Doc


data Value
  = VLam Text Value
  | VPi Text Value
  | VNeutral Text [Value]
  | VCon Text [Value]
  | VPrim PrimVal
  | VType
  | VLinear
  | VError String -- something bad happened!!
  deriving (Show)


instance Pretty Value where
  pretty = \case
    VLam v t      -> "\\" <+> pretty v <+> "."  <+> pretty t
    VPi v t       -> "\\" <+> pretty v <+> "->" <+> pretty t
    VNeutral v xs -> pretty v <+> hcat (pretty <$> xs)
    VCon n xs     -> pretty n <+> hcat (pretty <$> xs)
    VPrim v       -> pretty v
    VType         -> "Type"
    VLinear       -> "Linear"
    VError err    -> pretty err