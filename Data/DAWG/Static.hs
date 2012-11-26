{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
--
-- In comparison to "Data.DAWG" module the automaton implemented here:
--
--   * Keeps all nodes in one array and therefore uses much less memory,
--
--   * When 'weigh'ed, it can be used to perform static hashing with
--     'hash' and 'unHash' functions,
--
--   * Doesn't provide insert/delete family of operations.

module Data.DAWG.Static
(
-- * DAWG type
  DAWG
-- * Query
, lookup
, numStates
-- * Index
, index
, byIndex
-- * Hash
, hash
, unHash
-- * Construction
, empty
, fromList
, fromListWith
, fromLang
, freeze
-- * Weight
, Weight
, weigh
-- * Conversion
, assocs
, keys
, elems
-- , thaw
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Arrow (first)
import Data.Binary (Binary, put, get)
import Data.Vector.Binary ()
import Data.Vector.Unboxed (Unbox)
import qualified Data.IntMap as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.DAWG.Types
import Data.DAWG.Trans (Trans)
import qualified Data.DAWG.Trans as T
import qualified Data.DAWG.Trans.Vector as VT
import qualified Data.DAWG.Node as N
import qualified Data.DAWG.Graph as G
import qualified Data.DAWG.Internal as D
import qualified Data.DAWG.Util as Util

type Node t a b = N.Node t (Maybe a) b

class (Ord (Node t a b), Trans t) => MkNode t a b where
instance (Ord (Node t a b), Trans t) => MkNode t a b where

-- | @DAWG t a b c@ constitutes an automaton with alphabet symbols of type /a/,
-- node values of type /Maybe b/ and additional transition labels of type /c/.
-- Root is stored on the first position of the array.
newtype DAWG t a b c = DAWG { unDAWG :: V.Vector (Node t b c) }
    deriving (Show)

deriving instance (Eq b, Eq c, Unbox c)     => Eq  (DAWG VT.Trans a b c)
deriving instance (Ord b, Ord c, Unbox c)   => Ord (DAWG VT.Trans a b c)

instance (Binary t, Binary b, Binary c, Unbox c) => Binary (DAWG t a b c) where
    put = put . unDAWG
    get = DAWG <$> get

-- | Empty DAWG.
empty :: (Trans t, Unbox c) => DAWG t a b c
empty = DAWG $ V.fromList
    [ N.Branch 1 T.empty U.empty
    , N.Leaf Nothing ]

-- | Number of states in the automaton.
numStates :: DAWG t a b c -> Int
numStates = V.length . unDAWG

-- | Node with the given identifier.
nodeBy :: ID -> DAWG t a b c -> Node t b c
nodeBy i d = unDAWG d V.! i

-- | Value in leaf node with a given ID.
leafValue :: Node t b c -> DAWG t a b c -> Maybe b
leafValue n = N.value . nodeBy (N.eps n)

-- | Find value associated with the key.
lookup :: (Enum a, Trans t, Unbox c) => [a] -> DAWG t a b c -> Maybe b
lookup xs' =
    let xs = map fromEnum xs'
    in  lookup'I xs 0
{-# SPECIALIZE lookup
        :: (Trans t, Unbox c) => String
        -> DAWG t Char b c -> Maybe b #-}

lookup'I :: (Trans t, Unbox c) => [Sym] -> ID -> DAWG t a b c -> Maybe b
lookup'I []     i d = leafValue (nodeBy i d) d
lookup'I (x:xs) i d = case N.onSym x (nodeBy i d) of
    Just j  -> lookup'I xs j d
    Nothing -> Nothing

-- | Return all key/value pairs in the DAWG in ascending key order.
assocs :: (Enum a, Trans t, Unbox c) => DAWG t a b c -> [([a], b)]
assocs d = map (first (map toEnum)) (assocs'I 0 d)
{-# SPECIALIZE assocs
        :: (Trans t, Unbox c)
        => DAWG t Char b c -> [(String, b)] #-}

assocs'I :: (Trans t, Unbox c) => ID -> DAWG t a b c -> [([Sym], b)]
assocs'I i d =
    here ++ concatMap there (N.edges n)
  where
    n = nodeBy i d
    here = case leafValue n d of
        Just x  -> [([], x)]
        Nothing -> []
    there (x, j) = map (first (x:)) (assocs'I j d)

-- | Return all keys of the DAWG in ascending order.
keys :: (Enum a, Trans t, Unbox c) => DAWG t a b c -> [[a]]
keys = map fst . assocs
{-# SPECIALIZE keys :: (Trans t, Unbox c) => DAWG t Char b c -> [String] #-}

-- | Return all elements of the DAWG in the ascending order of their keys.
elems :: (Trans t, Unbox c) => DAWG t a b c -> [b]
elems = map snd . assocs'I 0

-- | Construct 'DAWG' from the list of (word, value) pairs.
-- First a 'D.DAWG' is created and then it is frozen using
-- the 'freeze' function.
fromList
    :: (Enum a, Ord (Node t b ()), Trans t)
    => [([a], b)] -> DAWG t a b ()
fromList = freeze . D.fromList
{-# SPECIALIZE fromList
        :: (Ord (Node t b ()), Trans t)
        => [(String, b)] -> DAWG t Char b () #-}

-- | Construct DAWG from the list of (word, value) pairs
-- with a combining function.  The combining function is
-- applied strictly. First a 'D.DAWG' is created and then
-- it is frozen using the 'freeze' function.
fromListWith
    :: (Enum a, Ord (Node t b ()), Trans t)
    => (b -> b -> b) -> [([a], b)] -> DAWG t a b ()
fromListWith f = freeze . D.fromListWith f
{-# SPECIALIZE fromListWith
        :: (Ord (Node t b ()), Trans t)
        => (b -> b -> b) -> [(String, b)] -> DAWG t Char b () #-}

-- | Make DAWG from the list of words.  Annotate each word with
-- the @()@ value.  First a 'D.DAWG' is created and then it is frozen
-- using the 'freeze' function.
fromLang 
    :: (Enum a, Ord (Node t () ()), Trans t)
    => [[a]] -> DAWG t a () ()
fromLang = freeze . D.fromLang
{-# SPECIALIZE fromLang
        :: (Ord (Node t () ()), Trans t)
        => [String] -> DAWG t Char () () #-}

-- | Weight of a node corresponds to the number of final states
-- reachable from the node.  Weight of an edge is a sum of weights
-- of preceding nodes outgoing from the same parent node.
type Weight = Int

-- | Compute node weights and store corresponding values in transition labels.
weigh :: Trans t => DAWG t a b c -> DAWG t a b Weight
weigh d = (DAWG . V.fromList)
    [ branch n ws
    | i <- [0 .. numStates d - 1]
    , let n  = nodeBy i d
    , let ws = accum (N.children n) ]
  where
    -- Branch with new weights.
    branch N.Branch{..} ws  = N.Branch eps transMap ws
    branch N.Leaf{..} _     = N.Leaf value
    -- In nodeWeight node weights are memoized.
    nodeWeight = ((V.!) . V.fromList) (map detWeight [0 .. numStates d - 1])
    -- Determine weight of the node.
    detWeight i = case nodeBy i d of
        N.Leaf w    -> maybe 0 (const 1) w
        n           -> sum . map nodeWeight $ allChildren n
    -- Weights for subsequent edges.
    accum = U.fromList . init . scanl (+) 0 . map nodeWeight
    -- Plain children and epsilon child. 
    allChildren n = N.eps n : N.children n

-- | Construct immutable version of the automaton.
freeze :: Trans t => D.DAWG t a b -> DAWG t a b ()
freeze d = DAWG . V.fromList $
    map (N.reID newID . oldBy)
        (M.elems (inverse old2new))
  where
    -- Map from old to new identifiers.
    old2new = M.fromList $ (D.root d, 0) : zip (nodeIDs d) [1..]
    newID   = (M.!) old2new
    -- List of node IDs without the root ID.
    nodeIDs = filter (/= D.root d) . map fst . M.assocs . G.nodeMap . D.graph
    -- Non-frozen node by given identifier.
    oldBy i = G.nodeBy i (D.graph d)
        
-- | Inverse of the map.
inverse :: M.IntMap Int -> M.IntMap Int
inverse =
    let swap (x, y) = (y, x)
    in  M.fromList . map swap . M.toList

-- -- | Yield mutable version of the automaton.
-- thaw :: (Unbox c, Ord a) => DAWG a b c -> D.DAWG a b
-- thaw d =
--     D.fromNodes nodes 0
--   where
--     -- List of resulting nodes.
--     nodes = branchNodes ++ leafNodes
--     -- Branching nodes.
--     branchNodes =
--         [ 
--     -- Number of states used to shift new value IDs.
--     n = numStates d
--     -- New identifiers for value nodes.
--     valIDs = foldl' updID GM.empty (values d)
--     -- Values in the automaton.
--     values = map value . V.toList . unDAWG
--     -- Update ID map.
--     updID m v = case GM.lookup v m of
--         Just i  -> m
--         Nothing -> 
--             let j = GM.size m + n
--             in  j `seq` GM.insert v j

-- | Position in a set of all dictionary entries with respect
-- to the lexicographic order.
index :: (Enum a, Trans t) => [a] -> DAWG t a b Weight -> Maybe Int
index xs = index'I (map fromEnum xs) 0
{-# SPECIALIZE index
        :: Trans t => String -> DAWG t Char b Weight -> Maybe Int #-}

index'I :: Trans t => [Sym] -> ID -> DAWG t a b Weight -> Maybe Int
index'I []     i d = 0 <$ leafValue (nodeBy i d) d
index'I (x:xs) i d = do
    let n = nodeBy i d
        u = maybe 0 (const 1) (leafValue n d)
    (j, v) <- N.onSym' x n
    w <- index'I xs j d
    return (u + v + w)

-- | Perfect hashing function for dictionary entries.
-- A synonym for the 'index' function.
hash :: (Enum a, Trans t) => [a] -> DAWG t a b Weight -> Maybe Int
hash = index
{-# INLINE hash #-}

-- | Find dictionary entry given its index with respect to the
-- lexicographic order.
byIndex :: (Enum a, Trans t) => Int -> DAWG t a b Weight -> Maybe [a]
byIndex ix d = map toEnum <$> byIndex'I ix 0 d
{-# SPECIALIZE byIndex
        :: Trans t => Int -> DAWG t Char b Weight -> Maybe String #-}

byIndex'I :: Trans t => Int -> ID -> DAWG t a b Weight -> Maybe [Sym]
byIndex'I ix i d
    | ix < 0    = Nothing
    | otherwise = here <|> there
  where
    n = nodeBy i d
    u = maybe 0 (const 1) (leafValue n d)
    here
        | ix == 0   = [] <$ leafValue (nodeBy i d) d
        | otherwise = Nothing
    there = do
        (k, w) <- Util.findLastLE cmp (N.labelVect n)
        (x, j) <- T.byIndex k (N.transMap n)
        xs <- byIndex'I (ix - u - w) j d
        return (x:xs)
    cmp w = compare w (ix - u)

-- | Inverse of the 'hash' function and a synonym for the 'byIndex' function.
unHash :: (Enum a, Trans t) => Int -> DAWG t a b Weight -> Maybe [a]
unHash = byIndex
{-# INLINE unHash #-}
