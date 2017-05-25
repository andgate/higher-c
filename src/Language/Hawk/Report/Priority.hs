module Language.Hawk.Report.Priority where

data Priority
    = None
    | Standard
    | Verbose
    deriving (Show, Eq, Ord)


class HasPriority a where
    priority :: a -> Priority


filterPriority :: HasPriority a => Priority -> [a] -> [a]
filterPriority = undefined