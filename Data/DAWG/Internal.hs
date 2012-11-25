-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.

module Data.DAWG.Internal
(
-- -- * DAWG type
--   DAWG (..)
-- -- * Query
-- , numStates
-- , lookup
-- -- * Construction
-- , empty
-- , fromList
-- , fromListWith
-- , fromLang
-- -- ** Insertion
-- , insert
-- , insertWith
-- -- ** Deletion
-- , delete
-- -- * Conversion
-- , assocs
-- , keys
-- , elems
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.List (foldl')
import Data.Binary (Binary, put, get)
import qualified Data.Vector.Unboxed as U
import qualified Control.Monad.State.Strict as S

import Data.DAWG.Types
import Data.DAWG.Graph (Graph)
import Data.DAWG.Node.Class (Node)
import qualified Data.DAWG.Node.Class as N
import qualified Data.DAWG.Graph as G
import qualified Data.DAWG.VMap as V

-- import Data.DAWG.Node hiding (Node)
-- import qualified Data.DAWG.Node as N

-- type Node a = N.Node (Maybe a) ()
-- 
-- type GraphM a b = S.State (Graph (Node a)) b
type GraphM n a b = S.State (Graph (n a)) b

mkState :: (Graph a -> Graph a) -> Graph a -> ((), Graph a)
mkState f g = ((), f g)

-- | Leaf node with no children and 'Nothing' value.
insertLeaf :: Node n a => GraphM n a ID 
insertLeaf = do
    i <- insertNode (N.leaf Nothing)
    insertNode (N.branch i)
-- insertLeaf :: Node n a => GraphM n a ID 
-- insertLeaf = do
--     i <- insertNode (N.Leaf Nothing)
--     insertNode (N.Branch i V.empty U.empty)

-- | Return node with the given identifier.
nodeBy :: Node n a => ID -> GraphM n a (n a)
nodeBy i = G.nodeBy i <$> S.get

-- Evaluate the 'G.insert' function within the monad.
insertNode :: Node n a => n a -> GraphM n a ID
insertNode = S.state . G.insert

-- Evaluate the 'G.delete' function within the monad.
deleteNode :: Node n a => n a -> GraphM n a ()
deleteNode = S.state . mkState . G.delete

-- | Invariant: the identifier points to the 'Branch' node.
insertM :: Node n a => [Sym] -> a -> ID -> GraphM n a ID
insertM (x:xs) y i = do
    n <- nodeBy i
    j <- case N.onSym x n of
        Just j  -> return j
        Nothing -> insertLeaf
    k <- insertM xs y j
    deleteNode n
    insertNode (N.subst x k n)
insertM [] y i = do
    n <- nodeBy i
    w <- nodeBy (N.eps n)
    deleteNode w
    deleteNode n
    j <- insertNode (N.leaf $ Just y)
    insertNode (N.withEps j n)

insertWithM :: Node n a => (a -> a -> a) -> [Sym] -> a -> ID -> GraphM n a ID
insertWithM f (x:xs) y i = do
    n <- nodeBy i
    j <- case N.onSym x n of
        Just j  -> return j
        Nothing -> insertLeaf
    k <- insertWithM f xs y j
    deleteNode n
    insertNode (N.subst x k n)
insertWithM f [] y i = do
    n <- nodeBy i
    w <- nodeBy (N.eps n)
    deleteNode w
    deleteNode n
    let y'new = case N.value w of
            Just y' -> f y y'
            Nothing -> y
    j <- insertNode (N.leaf $ Just y'new)
    insertNode (N.withEps j n)

deleteM :: Node n a => [Sym] -> ID -> GraphM n a ID
deleteM (x:xs) i = do
    n <- nodeBy i
    case N.onSym x n of
        Nothing -> return i
        Just j  -> do
            k <- deleteM xs j
            deleteNode n
            insertNode (N.subst x k n)
deleteM [] i = do
    n <- nodeBy i
    w <- nodeBy (N.eps n)
    deleteNode w
    deleteNode n
    j <- insertLeaf
    insertNode (N.withEps j n)
    
lookupM :: Node n a => [Sym] -> ID -> GraphM n a (Maybe a)
lookupM [] i = do
    j <- N.eps <$> nodeBy i
    N.value <$> nodeBy j
lookupM (x:xs) i = do
    n <- nodeBy i
    case N.onSym x n of
        Just j  -> lookupM xs j
        Nothing -> return Nothing

assocsAcc :: Node n a => Graph (n a) -> ID -> [([Sym], a)]
assocsAcc g i =
    here w ++ concatMap there (N.edges n)
  where
    n = G.nodeBy i g
    w = G.nodeBy (N.eps n) g
    here v = case N.value v of
        Just x  -> [([], x)]
        Nothing -> []
    there (sym, j) = map (first (sym:)) (assocsAcc g j)

-- | A directed acyclic word graph with phantom type @a@ representing
-- type of alphabet elements.
data DAWG a n b = DAWG
    { graph :: !(Graph (n b))
    , root  :: !ID }
    deriving (Show, Eq, Ord)

instance Node n b => Binary (DAWG a n b) where
    put d = do
        put (graph d)
        put (root d)
    get = DAWG <$> get <*> get

-- | Empty DAWG.
empty :: Node n b => DAWG a n b
empty = 
    let (i, g) = S.runState insertLeaf G.empty
    in  DAWG g i

-- | Number of states in the underlying graph.
numStates :: DAWG a n b -> Int
numStates = G.size . graph

-- | Insert the (key, value) pair into the DAWG.
insert :: (Enum a, Node n b) => [a] -> b -> DAWG a n b -> DAWG a n b
insert xs' y d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (insertM xs y $ root d) (graph d)
    in  DAWG g i
{-# INLINE insert #-}
{-# SPECIALIZE insert
        :: Node n b => String -> b
        -> DAWG Char n b -> DAWG Char n b #-}

-- | Insert with a function, combining new value and old value.
-- 'insertWith' f key value d will insert the pair (key, value) into d if
-- key does not exist in the DAWG. If the key does exist, the function
-- will insert the pair (key, f new_value old_value).
insertWith
    :: (Enum a, Node n b) => (b -> b -> b)
    -> [a] -> b -> DAWG a n b -> DAWG a n b
insertWith f xs' y d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (insertWithM f xs y $ root d) (graph d)
    in  DAWG g i
{-# SPECIALIZE insertWith
        :: Node n b => (b -> b -> b) -> String -> b
        -> DAWG Char n b -> DAWG Char n b #-}

-- | Delete the key from the DAWG.
delete :: (Enum a, Node n b) => [a] -> DAWG a n b -> DAWG a n b
delete xs' d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (deleteM xs $ root d) (graph d)
    in  DAWG g i
{-# SPECIALIZE delete
        :: Node n b => String -> DAWG Char n b
        -> DAWG Char n b #-}

-- | Find value associated with the key.
lookup :: (Enum a, Node n b) => [a] -> DAWG a n b -> Maybe b
lookup xs' d =
    let xs = map fromEnum xs'
    in  S.evalState (lookupM xs $ root d) (graph d)
{-# SPECIALIZE lookup :: Node n b => String -> DAWG Char n b -> Maybe b #-}

-- | Return all key/value pairs in the DAWG in ascending key order.
assocs :: (Enum a, Node n b) => DAWG a n b -> [([a], b)]
assocs
    = map (first (map toEnum))
    . (assocsAcc <$> graph <*> root)
{-# SPECIALIZE assocs :: Node n b => DAWG Char n b -> [(String, b)] #-}

-- | Return all keys of the DAWG in ascending order.
keys :: Enum a => DAWG a b -> [[a]]
keys = map fst . assocs
{-# SPECIALIZE keys :: DAWG Char b -> [String] #-}

-- -- | Return all elements of the DAWG in the ascending order of their keys.
-- elems :: DAWG a b -> [b]
-- elems = map snd . (assocsAcc <$> graph <*> root)
-- 
-- -- | Construct DAWG from the list of (word, value) pairs.
-- fromList :: (Enum a, Ord b) => [([a], b)] -> DAWG a b
-- fromList xs =
--     let update t (x, v) = insert x v t
--     in  foldl' update empty xs
-- {-# INLINE fromList #-}
-- {-# SPECIALIZE fromList :: Ord b => [(String, b)] -> DAWG Char b #-}
-- 
-- -- | Construct DAWG from the list of (word, value) pairs
-- -- with a combining function.  The combining function is
-- -- applied strictly.
-- fromListWith :: (Enum a, Ord b) => (b -> b -> b) -> [([a], b)] -> DAWG a b
-- fromListWith f xs =
--     let update t (x, v) = insertWith f x v t
--     in  foldl' update empty xs
-- {-# SPECIALIZE fromListWith :: Ord b => (b -> b -> b)
--         -> [(String, b)] -> DAWG Char b #-}
-- 
-- -- | Make DAWG from the list of words.  Annotate each word with
-- -- the @()@ value.
-- fromLang :: Enum a => [[a]] -> DAWG a ()
-- fromLang xs = fromList [(x, ()) | x <- xs]
-- {-# SPECIALIZE fromLang :: [String] -> DAWG Char () #-}
