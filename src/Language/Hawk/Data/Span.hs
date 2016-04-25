module Language.Hawk.Data.Span where

import Data.Generics
import Data.Monoid


-- | uniform representation of source file positions
data Span = Span { spanStartRow     :: Int     -- ^ row (line)  in the original file. Affected by #LINE pragmas.
                 , spanEndRow       :: Int     -- ^ row (line)  in the original file. Affected by #LINE pragmas.
                 , spanStartColumn  :: Int  -- ^ column in the preprocessed file. Inaccurate w.r.t. to the original
                 , spanEndColumn    :: Int  -- ^ column in the preprocessed file. Inaccurate w.r.t. to the original
                 }
                deriving (Show, Eq, Ord)

{-
instance Show Span where
  show (Span row_1 row_2 col_1 col_2) = show row_1 ++ ":" ++ show col_1 ++ "-" ++ show row_2 ++ ":" ++ show col_2
-}
  
instance Monoid Span where
  mempty = Span 0 0 0 0
  mappend (Span a_row_1 a_row_2 a_col_1 a_col_2) (Span b_row_1 b_row_2 b_col_1 b_col_2)
    = Span (min a_row_1 b_row_1) (max a_row_2 b_row_2) (min a_col_1 b_col_1) (max a_col_2 b_col_2)


-- | class of type which aggregate a source code location
class HkSpan a where
    spanOf :: a -> Span