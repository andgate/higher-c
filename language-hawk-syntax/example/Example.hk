--module Core has


-- Primitive Types
-- Numeric: Bit, Nat, Int, Real
-- Text:    Char, String
-- Array:   [x]
-- Memory:  Id, Cell, Box, Linear, Ref, Lazy, GC, IO
--
-- | Built-in Monads | --
-- Id = constant value, stack (default) or heap (via Box)
--   read
-- Cell = Mutable Value, stack (default) or heap (via Box)
--   read, write, copy
-- Box = constant value, heap only, must be deleted
-- Linear = constant value, read only and destroy on read, heap only
-- Ref = Pointer to some object to an address.
-- Lazy = Can be combined with other lazy functions and evaluated later.
-- GC = Garbage collected values (will force gc to be included)
        -- gc'd values are heap-only
        -- also, gc values don't have to be managed
        -- GC is slow, so add tag 
-- IO = Operation external to program. All monads that communicate
--      externally, like with OS, filesystem, or network, must go
--      through IO. They may wrap IO if they wish, but IO is the core
--      Monad they are built on.

-- Core library will have:
--   - Category Theory
--   - Graph Algorithms
--   - Linear Algebra
--   - Filesystem



class Assigns f has
  (=) :  f a  -- Destination
      -> f a  -- Source
      -> f a  -- Destination

Assigns Cell has
  (=) :  Cell a  -- Destination
      -> Cell a  -- Source
      -> Cell a  -- Destination


-- New memory model
--  Needs:
--    1)  A Single Constructor
--    2)  Magical unboxing algorithm
            -- intelligently organizes data
            --  between heap and stack.
--
--  Mutable, touchable objects ARE HARD.
--  Category theory doesn't work well with them.
--  What if delete was like (expr ; names)

-- Need to import the following from C...
--   putStr()

foreign import ccall putStr : String -> ()

-- Optimizes to a single bit, for single bit if checks
Bool = True | False

-- Id is basic stack storage
Id a = Id { id : a }

-- Cell a = Cell some
Cell a = Cell { mutable : a }

-- Box allows for heap storage
Box a = Box { modify : a }

-- Some pointer to some region of data
Ref a = Ref { deref : a }

-- Shared pointer
Safe a = Safe { unsafe : a }

 

-- Linear heap storage. If it is copied, it is destroyed!
Linear a = Linear { consume : a }

-- Classic functiional types
Maybe a = Nothing | Just a
Either a b = Left a | Right b


-- ----------------------------------
-- Category Laws
-- 1.  f . id  ==  f    (right identity)
-- 2.  id . f  ==  f    (left identity)
-- 3.  f . (g . h) == (f . g) . h   (associativity)
-------------------------------------

($) : (a -> b) 

class Category cat where
  id : cat a a
  (.) : cat 

Category (->) where
  id = \a -> a
  (.) g f a = g (f a) 


-------------------------------------
-- Functor Laws
-- 1.  fmap id  ==  id
-- 2.  fmap (f . g)  ==  fmap f . fmap g
-------------------------------------

-- Functor Class
class Functor f has
  fmap : (a -> b) -> f a -> f b


-- Functor Instances
Functor Box has
  fmap f (Box x) = Box (f x)
  
Functor Linear has
  fmap f (Linear x) = Linear (f x)

Functor Maybe has
  fmap f Nothing = Nothing
  fmap f (Just a) = Just (f a)

Functor Either a has
  fmap f e =
    case e of
      Left x -> Left x
      Right a -> Right a

Functor [a] has
  fmap f xs =
    let xs' <- [] 5 : [b]
        loop : Int -> [b]
        loop 0 = xs' 
        loop i = 
            let x' <- f xs[i]
                xs'[i] = x
            in fmap' (xs'(i-1)
    in loop (size xs)
    
    xs' <- Arr (size xs) []
    go <- ( \i -> xs'[i] = f xs[i]; go (i-1))  
    go (size xs)
    xs'


-- Applicative Class
class Functor f => Applicative f has
  pure : a -> f a
  (<*>) : f (a -> b) -> f a -> f b

-- Applicative Instances

Applicative Box has
  pure a = Box a
  (<*>) (Box f) m = fmap f m

Applicative Linear has
  pure a = Linear a
  (<*>) (Linear f) m = fmap f m
  
Applicative Maybe has
  pure a = Just a

  (<*>) (Just f) m = fmap f m
  (<*>) Nothing _  = Nothing
  
Applicative Either e where
  pure a = Right a
  
  (<*>) (Right f) m = fmap f m
  (<*>) (Left e)  _ = Left e


-- Monad Class
class Applicative m => Monad m a has
  (>>=) : m a -> (a -> m b) -> m b

  (>>) : m a -> m b -> m b
  (>>) m k = m >>= \_ -> k
  
  return : a -> m a
  return = pure


-- Monad Instances
Monad Box has
  (Box x) >>= k = k x

Monad Linear has
  (Linear x) >>= k = k x
  
Monad Maybe has
  (Just x) >>= k = k x
  Nothing >>= _ = Nothing

Monad Either e has
  (Left e) >>= k = Left e
  (Right x) >>= k = k x
  
  
-- Graph stuff

Rep
  = AdjecencyList
  | EdgeList
  | Matrix
  
IntGraph = IntGraph { edges : [[Int]] }

class Graph g has
  size : g -> Int
  bfs : g -> Int -> ([Int], [Int], [Int])
  
  
Graph IntGraph has
  size g = size . edges $ g
  bfs g s = do
    par <- Array (size g) : [Int]
    dist <- Array (size g) : [Int]
    q <- Array (size g) : [Int]
    
     
    
    
    
    return ([], [], [])
    
  


-- IO

readFile : String -> IO String


-- Super fast, store entire game state in a 
-- mutable cell for easy save states.
-- Also needs to IO for loading levels
-- and networking.
myGame : CellT IO ()