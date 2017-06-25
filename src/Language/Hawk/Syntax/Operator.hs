module Language.Hawk.Syntax.Operator where

import Data.Binary
import Data.Text.Lazy (pack)
import qualified Text.PrettyPrint.Leijen.Text as PP

-- -----------------------------------------------------------------------------
-- | Types
--   Based on the C operators, for now

data UnaryOp
    = PreIncOp
    | PreDecOp
    | PostIncOp
    | PostDecOp
    | AdrOp -- address operator
    | IndOp -- indirection operator
    | PrePlusOp
    | PreMinOp
    | CompOp -- One's Complement
    | NegOp -- Logical negation
    deriving (Show, Eq, Ord, Enum)

data BinaryOp
    = AccessOp -- Data member access
    | MulOp
    | DivOp
    | RmdOp
    | AddOp
    | SubOp
    | ShlOp -- Shift left
    | ShrOp -- Shift right
    | LeOp
    | GrOp
    | LeqOp
    | GeqOp
    | EqOp
    | NeqOp
    | AndOp -- bitwise and
    | XorOp -- bitwise xor
    | OrOp -- bitwise or
    | LndOp -- Logical and
    | LorOp -- logical or
    | IndexOp -- Array index
    deriving (Show, Eq, Ord, Enum)


data AssignOp
    = AssignOp
    | MulAssOp
    | DivAssOp
    | RmdAssOp
    | AddAssOp
    | SubAssOp
    | ShlAssOp
    | ShrAssOp
    | AndAssOp
    | XorAssOp
    | OrAssOp
    deriving (Show, Eq, Ord, Enum)

-- -----------------------------------------------------------------------------
-- | Instances

-- Pretty ---------------------------------------------------------------------


instance PP.Pretty UnaryOp where
    pretty =
      PP.text . pack . show

instance PP.Pretty BinaryOp where
    pretty =
      PP.text . pack . show

instance PP.Pretty AssignOp where
    pretty =
      PP.text . pack . show


-- Binary ---------------------------------------------------------------------

instance Binary UnaryOp where
  get =
    toEnum . fromIntegral <$> getWord8
      
  put =
    putWord8 . fromIntegral . fromEnum


instance Binary BinaryOp where
  get =
    toEnum . fromIntegral <$> getWord8
      
  put =
    putWord8 . fromIntegral . fromEnum


instance Binary AssignOp where
  get =
    toEnum . fromIntegral <$> getWord8
      
  put =
    putWord8 . fromIntegral . fromEnum