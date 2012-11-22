-- | The module implements of /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.

module Data.DAWG
(
-- * DAWG type
  DAWG (..)
-- * Query
, numStates
, lookup
-- * Construction
, empty
-- ** Insertion
, insert
, insertWith
-- ** Deletion
, delete
-- * Conversion
, elems
, keys
, assocs
, fromList
, fromListWith
, fromLang
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.List (foldl')
import Data.Binary (Binary, put, get)
import qualified Control.Monad.State.Strict as S

import Data.DAWG.Graph (Id, Node, Graph)
import qualified Data.DAWG.Graph as G
import qualified Data.DAWG.VMap as V

type GraphM a b = S.State (Graph (Maybe a)) b

mkState :: (Graph a -> Graph a) -> Graph a -> ((), Graph a)
mkState f g = ((), f g)

-- | Leaf node with no children and 'Nothing' value.
insertLeaf :: Ord a => GraphM a Id 
insertLeaf = do
    i <- insertNode (G.Value Nothing)
    insertNode (G.Branch i V.empty)

-- | Return node with the given identifier.
nodeBy :: Id -> GraphM a (Node (Maybe a))
nodeBy i = G.nodeBy i <$> S.get

-- Evaluate the 'G.insert' function within the monad.
insertNode :: Ord a => Node (Maybe a) -> GraphM a Id
insertNode = S.state . G.insert

-- Evaluate the 'G.delete' function within the monad.
deleteNode :: Ord a => Node (Maybe a) -> GraphM a ()
deleteNode = S.state . mkState . G.delete

-- | Invariant: the identifier points to the 'Branch' node.
insertM :: Ord a => [Int] -> a -> Id -> GraphM a Id
insertM (x:xs) y i = do
    n <- nodeBy i
    j <- case G.onChar x n of
        Just j  -> return j
        Nothing -> insertLeaf
    k <- insertM xs y j
    deleteNode n
    insertNode (G.subst x k n)
insertM [] y i = do
    n <- nodeBy i
    w <- nodeBy (G.eps n)
    deleteNode w
    deleteNode n
    j <- insertNode (G.Value $ Just y)
    insertNode (n { G.eps = j })

insertWithM :: Ord a => (a -> a -> a) -> [Int] -> a -> Id -> GraphM a Id
insertWithM f (x:xs) y i = do
    n <- nodeBy i
    j <- case G.onChar x n of
        Just j  -> return j
        Nothing -> insertLeaf
    k <- insertWithM f xs y j
    deleteNode n
    insertNode (G.subst x k n)
insertWithM f [] y i = do
    n <- nodeBy i
    w <- nodeBy (G.eps n)
    deleteNode w
    deleteNode n
    let y'new = case G.unValue w of
            Just y' -> f y y'
            Nothing -> y
    j <- insertNode (G.Value $ Just y'new)
    insertNode (n { G.eps = j })

deleteM :: Ord a => [Int] -> Id -> GraphM a Id
deleteM (x:xs) i = do
    n <- nodeBy i
    case G.onChar x n of
        Nothing -> return i
        Just j  -> do
            k <- deleteM xs j
            deleteNode n
            insertNode (G.subst x k n)
deleteM [] i = do
    n <- nodeBy i
    w <- nodeBy (G.eps n)
    deleteNode w
    deleteNode n
    j <- insertLeaf
    insertNode (n { G.eps = j })
    
lookupM :: [Int] -> Id -> GraphM a (Maybe a)
lookupM [] i = do
    j <- G.eps <$> nodeBy i
    G.unValue <$> nodeBy j
lookupM (x:xs) i = do
    n <- nodeBy i
    case G.onChar x n of
        Just j  -> lookupM xs j
        Nothing -> return Nothing

assocsAcc :: Graph (Maybe a) -> Id -> [([Int], a)]
assocsAcc g i =
    here w ++ concatMap there (G.edges n)
  where
    n = G.nodeBy i g
    w = G.nodeBy (G.eps n) g
    here v = case G.unValue v of
        Just x  -> [([], x)]
        Nothing -> []
    there (char, j) = map (first (char:)) (assocsAcc g j)

-- | A 'G.Graph' with one root from which all other graph nodes should
-- be accesible.  Parameter @a@ is a phantom parameter and it represents
-- character type.
data DAWG a b = DAWG
    { graph :: !(Graph (Maybe b))
    , root  :: !Id }
    deriving (Show, Eq, Ord)

instance (Ord b, Binary b) => Binary (DAWG a b) where
    put d = do
        put (graph d)
        put (root d)
    get = DAWG <$> get <*> get

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
lookup :: Enum a => [a] -> DAWG a b -> Maybe b
lookup xs' d =
    let xs = map fromEnum xs'
    in  S.evalState (lookupM xs $ root d) (graph d)
{-# SPECIALIZE lookup :: String -> DAWG Char b -> Maybe b #-}

-- | Return all keys of the DAWG in ascending order.
keys :: Enum a => DAWG a b -> [[a]]
keys = map fst . assocs
{-# SPECIALIZE keys :: DAWG Char b -> [String] #-}

-- | Return all elements of the DAWG in the ascending order of their keys.
elems :: Enum a => DAWG a b -> [b]
elems = map snd . assocs
{-# SPECIALIZE elems :: DAWG Char b -> [b] #-}

-- | Return all key/value pairs in the DAWG in ascending key order.
assocs :: Enum a => DAWG a b -> [([a], b)]
assocs d
    = map (first (map toEnum))
    $ assocsAcc (graph d) (root d)
{-# SPECIALIZE assocs :: DAWG Char b -> [(String, b)] #-}

-- | Construct DAWG from the list of (word, value) pairs.
fromList :: (Enum a, Ord b) => [([a], b)] -> DAWG a b
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs
{-# SPECIALIZE fromList :: Ord b => [(String, b)] -> DAWG Char b #-}

-- | Construct DAWG from the list of (word, value) pairs
-- with a combining function.  The combining function is
-- applied strictly.
fromListWith :: (Enum a, Ord b) => (b -> b -> b) -> [([a], b)] -> DAWG a b
fromListWith f xs =
    let update t (x, v) = insertWith f x v t
    in  foldl' update empty xs
{-# SPECIALIZE fromListWith :: Ord b => (b -> b -> b)
        -> [(String, b)] -> DAWG Char b #-}

-- | Make DAWG from the list of words.  Annotate each word with
-- the @()@ value.
fromLang :: Enum a => [[a]] -> DAWG a ()
fromLang xs = fromList [(x, ()) | x <- xs]
{-# SPECIALIZE fromLang :: [String] -> DAWG Char () #-}
