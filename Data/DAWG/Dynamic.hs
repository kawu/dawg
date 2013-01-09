-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.

module Data.DAWG.Dynamic
(
-- * DAWG type
  DAWG
-- * Query
, numStates
, lookup
-- * Construction
, empty
, fromList
, fromListWith
, fromLang
-- ** Insertion
, insert
, insertWith
-- ** Deletion
, delete
-- * Conversion
, assocs
, keys
, elems
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.List (foldl')
import qualified Control.Monad.State.Strict as S

import Data.DAWG.Types
import Data.DAWG.Graph (Graph)
import Data.DAWG.Dynamic.Internal
import qualified Data.DAWG.Trans as T
import qualified Data.DAWG.Graph as G
import qualified Data.DAWG.Dynamic.Node as N

type GraphM a b = S.State (Graph (N.Node a)) b

mkState :: (Graph a -> Graph a) -> Graph a -> ((), Graph a)
mkState f g = ((), f g)

-- | Return node with the given identifier.
nodeBy :: ID -> GraphM a (N.Node a)
nodeBy i = G.nodeBy i <$> S.get

-- Evaluate the 'G.insert' function within the monad.
insertNode :: Ord a => N.Node a -> GraphM a ID
insertNode = S.state . G.insert

-- | Leaf node with no children and 'Nothing' value.
insertLeaf :: Ord a => GraphM a ID
insertLeaf = do
    i <- insertNode (N.Leaf Nothing)
    insertNode (N.Branch i T.empty)

-- Evaluate the 'G.delete' function within the monad.
deleteNode :: Ord a => N.Node a -> GraphM a ()
deleteNode = S.state . mkState . G.delete

-- | Invariant: the identifier points to the 'Branch' node.
insertM :: Ord a => [Sym] -> a -> ID -> GraphM a ID
insertM (x:xs) y i = do
    n <- nodeBy i
    j <- case N.onSym x n of
        Just j  -> return j
        Nothing -> insertLeaf
    k <- insertM xs y j
    deleteNode n
    insertNode (N.insert x k n)
insertM [] y i = do
    n <- nodeBy i
    w <- nodeBy (N.eps n)
    deleteNode w
    deleteNode n
    j <- insertNode (N.Leaf $ Just y)
    insertNode (n { N.eps = j })

insertWithM
    :: Ord a => (a -> a -> a)
    -> [Sym] -> a -> ID -> GraphM a ID
insertWithM f (x:xs) y i = do
    n <- nodeBy i
    j <- case N.onSym x n of
        Just j  -> return j
        Nothing -> insertLeaf
    k <- insertWithM f xs y j
    deleteNode n
    insertNode (N.insert x k n)
insertWithM f [] y i = do
    n <- nodeBy i
    w <- nodeBy (N.eps n)
    deleteNode w
    deleteNode n
    let y'new = case N.value w of
            Just y' -> f y y'
            Nothing -> y
    j <- insertNode (N.Leaf $ Just y'new)
    insertNode (n { N.eps = j })

deleteM :: Ord a => [Sym] -> ID -> GraphM a ID
deleteM (x:xs) i = do
    n <- nodeBy i
    case N.onSym x n of
        Nothing -> return i
        Just j  -> do
            k <- deleteM xs j
            deleteNode n
            insertNode (N.insert x k n)
deleteM [] i = do
    n <- nodeBy i
    w <- nodeBy (N.eps n)
    deleteNode w
    deleteNode n
    j <- insertLeaf
    insertNode (n { N.eps = j })
    
lookupM :: [Sym] -> ID -> GraphM a (Maybe a)
lookupM [] i = do
    j <- N.eps <$> nodeBy i
    N.value <$> nodeBy j
lookupM (x:xs) i = do
    n <- nodeBy i
    case N.onSym x n of
        Just j  -> lookupM xs j
        Nothing -> return Nothing

assocsAcc :: Graph (N.Node a) -> ID -> [([Sym], a)]
assocsAcc g i =
    here w ++ concatMap there (N.edges n)
  where
    n = G.nodeBy i g
    w = G.nodeBy (N.eps n) g
    here v = case N.value v of
        Just x  -> [([], x)]
        Nothing -> []
    there (sym, j) = map (first (sym:)) (assocsAcc g j)

-- | Empty DAWG.
empty :: Ord b => DAWG a b
empty = 
    let (i, g) = S.runState insertLeaf G.empty
    in  DAWG g i

-- | Number of states in the underlying graph.
numStates :: DAWG a b -> Int
numStates = G.size . graph

-- | Insert the (key, value) pair into the DAWG.
insert :: (Enum a, Ord b) => [a] -> b -> DAWG a b -> DAWG a b
insert xs' y d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (insertM xs y $ root d) (graph d)
    in  DAWG g i
{-# INLINE insert #-}
{-# SPECIALIZE insert :: Ord b => String -> b -> DAWG Char b -> DAWG Char b #-}

-- | Insert with a function, combining new value and old value.
-- 'insertWith' f key value d will insert the pair (key, value) into d if
-- key does not exist in the DAWG. If the key does exist, the function
-- will insert the pair (key, f new_value old_value).
insertWith
    :: (Enum a, Ord b) => (b -> b -> b)
    -> [a] -> b -> DAWG a b -> DAWG a b
insertWith f xs' y d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (insertWithM f xs y $ root d) (graph d)
    in  DAWG g i
{-# SPECIALIZE insertWith
        :: Ord b => (b -> b -> b) -> String -> b
        -> DAWG Char b -> DAWG Char b #-}

-- | Delete the key from the DAWG.
delete :: (Enum a, Ord b) => [a] -> DAWG a b -> DAWG a b
delete xs' d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (deleteM xs $ root d) (graph d)
    in  DAWG g i
{-# SPECIALIZE delete :: Ord b => String -> DAWG Char b -> DAWG Char b #-}

-- | Find value associated with the key.
lookup :: (Enum a, Ord b) => [a] -> DAWG a b -> Maybe b
lookup xs' d =
    let xs = map fromEnum xs'
    in  S.evalState (lookupM xs $ root d) (graph d)
{-# SPECIALIZE lookup :: Ord b => String -> DAWG Char b -> Maybe b #-}

-- | Return all key/value pairs in the DAWG in ascending key order.
assocs :: (Enum a, Ord b) => DAWG a b -> [([a], b)]
assocs
    = map (first (map toEnum))
    . (assocsAcc <$> graph <*> root)
{-# SPECIALIZE assocs :: Ord b => DAWG Char b -> [(String, b)] #-}

-- | Return all keys of the DAWG in ascending order.
keys :: (Enum a, Ord b) => DAWG a b -> [[a]]
keys = map fst . assocs
{-# SPECIALIZE keys :: Ord b => DAWG Char b -> [String] #-}

-- | Return all elements of the DAWG in the ascending order of their keys.
elems :: Ord b => DAWG a b -> [b]
elems = map snd . (assocsAcc <$> graph <*> root)

-- | Construct DAWG from the list of (word, value) pairs.
fromList :: (Enum a, Ord b) => [([a], b)] -> DAWG a b
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs
{-# INLINE fromList #-}
{-# SPECIALIZE fromList :: Ord b => [(String, b)] -> DAWG Char b #-}

-- | Construct DAWG from the list of (word, value) pairs
-- with a combining function.  The combining function is
-- applied strictly.
fromListWith
    :: (Enum a, Ord b) => (b -> b -> b)
    -> [([a], b)] -> DAWG a b
fromListWith f xs =
    let update t (x, v) = insertWith f x v t
    in  foldl' update empty xs
{-# SPECIALIZE fromListWith
        :: Ord b => (b -> b -> b)
        -> [(String, b)] -> DAWG Char b #-}

-- | Make DAWG from the list of words.  Annotate each word with
-- the @()@ value.
fromLang :: Enum a => [[a]] -> DAWG a ()
fromLang xs = fromList [(x, ()) | x <- xs]
{-# SPECIALIZE fromLang :: [String] -> DAWG Char () #-}
