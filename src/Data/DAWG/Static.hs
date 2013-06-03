{-# LANGUAGE RecordWildCards #-}


-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
--
-- In comparison to "Data.DAWG.Dynamic" module the automaton implemented here:
--
--   * Keeps all nodes in one array and therefore uses less memory,
--
--   * When 'weigh'ed, it can be used to perform static hashing with
--     'index' and 'byIndex' functions,
--
--   * Doesn't provide insert/delete family of operations.


module Data.DAWG.Static
(
-- * DAWG type
  DAWG

-- * Query
, lookup
, submap
, numStates
, numEdges

-- * Weight
, Weight
, weigh
, size
, index
, byIndex

-- * Construction
, empty
, fromList
, fromListWith
, fromLang

-- * Conversion
, assocs
, keys
, elems
, freeze
-- , thaw
) where


import Prelude hiding (lookup)
import Control.Applicative ((<$), (<$>), (<*>), (<|>))
import Control.Arrow (first)
import Data.Binary (Binary, put, get)
import Data.Vector.Binary ()
import Data.Vector.Unboxed (Unbox)
import qualified Data.IntMap as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.DAWG.Types
import qualified Data.DAWG.Util as Util
import qualified Data.DAWG.Trans as T
import qualified Data.DAWG.Static.Node as N
import qualified Data.DAWG.Graph as G
import qualified Data.DAWG.Dynamic as D
import qualified Data.DAWG.Dynamic.Internal as D


-- | @DAWG a b c@ constitutes an automaton with alphabet symbols of type /a/,
-- transition labels of type /b/ and node values of type /Maybe c/.
-- All nodes are stored in a 'V.Vector' with positions of nodes corresponding
-- to their 'ID's.
--
data DAWG a b c = DAWG
    { unDAWG :: V.Vector (N.Node b c)
    -- | The actual DAWG root has the 0 ID.  Thanks to the 'root' attribute,
    -- we can represent a submap of the DAWG.
    , root   :: ID
    } deriving (Show, Eq, Ord)

instance (Binary b, Binary c, Unbox b) => Binary (DAWG a b c) where
    put DAWG{..} = put unDAWG >> put root
    get = DAWG <$> get <*> get


-- | Empty DAWG.
empty :: Unbox b => DAWG a b c
empty = flip DAWG 0 $ V.fromList
    [ N.Branch 1 T.empty U.empty
    , N.Leaf Nothing ]


-- | Return the sub-DAWG containing all keys beginning with a prefix.
-- The in-memory representation of the resultant DAWG is the same as of
-- the original one, only the pointer to the DAWG root will be different.
submap :: (Enum a, Unbox b) => [a] -> DAWG a b c -> DAWG a b c
submap xs d = case follow (map fromEnum xs) (root d) d of
    Just i  -> DAWG (unDAWG d) i 
    Nothing -> empty
{-# SPECIALIZE submap :: Unbox b => String -> DAWG Char b c -> DAWG Char b c #-}


-- | Number of states in the automaton.
-- TODO: The function ignores the `root` value, it won't work properly
-- after using the `submap` function.
numStates :: DAWG a b c -> Int
numStates = V.length . unDAWG


-- | Number of edges in the automaton.
-- TODO: The function ignores the `root` value, it won't work properly
-- after using the `submap` function.
numEdges :: DAWG a b c -> Int
numEdges = sum . map (length . N.edges) . V.toList . unDAWG


-- | Node with the given identifier.
nodeBy :: ID -> DAWG a b c -> N.Node b c
nodeBy i d = unDAWG d V.! i


-- | Value in leaf node with a given ID.
leafValue :: N.Node b c -> DAWG a b c -> Maybe c
leafValue n = N.value . nodeBy (N.eps n)


-- | Follow the path from the given identifier.
follow :: Unbox b => [Sym] -> ID -> DAWG a b c -> Maybe ID
follow (x:xs) i d = do
    j <- N.onSym x (nodeBy i d)
    follow xs j d
follow [] i _ = Just i


-- | Find value associated with the key.
lookup :: (Enum a, Unbox b) => [a] -> DAWG a b c -> Maybe c
lookup xs d = lookup'I (map fromEnum xs) (root d) d
{-# SPECIALIZE lookup :: Unbox b => String -> DAWG Char b c -> Maybe c #-}


lookup'I :: Unbox b => [Sym] -> ID -> DAWG a b c -> Maybe c
lookup'I xs i d = do
    j <- follow xs i d
    leafValue (nodeBy j d) d


-- -- | Find all (key, value) pairs such that key is prefixed
-- -- with the given string.
-- withPrefix :: (Enum a, Unbox b) => [a] -> DAWG a b c -> [([a], c)]
-- withPrefix xs d = maybe [] id $ do
--     i <- follow (map fromEnum xs) 0 d
--     let prepare = (xs ++) . map toEnum
--     return $ map (first prepare) (subPairs i d)
-- {-# SPECIALIZE withPrefix
--     :: Unbox b => String -> DAWG Char b c
--     -> [(String, c)] #-}


-- | Return all (key, value) pairs in ascending key order in the
-- sub-DAWG determined by the given node ID.
subPairs :: Unbox b => ID -> DAWG a b c -> [([Sym], c)]
subPairs i d =
    here ++ concatMap there (N.edges n)
  where
    n = nodeBy i d
    here = case leafValue n d of
        Just x  -> [([], x)]
        Nothing -> []
    there (x, j) = map (first (x:)) (subPairs j d)


-- | Return all (key, value) pairs in the DAWG in ascending key order.
assocs :: (Enum a, Unbox b) => DAWG a b c -> [([a], c)]
assocs d = map (first (map toEnum)) (subPairs (root d) d)
{-# SPECIALIZE assocs :: Unbox b => DAWG Char b c -> [(String, c)] #-}


-- | Return all keys of the DAWG in ascending order.
keys :: (Enum a, Unbox b) => DAWG a b c -> [[a]]
keys = map fst . assocs
{-# SPECIALIZE keys :: Unbox b => DAWG Char b c -> [String] #-}


-- | Return all elements of the DAWG in the ascending order of their keys.
elems :: Unbox b => DAWG a b c -> [c]
elems d = map snd $ subPairs (root d) d


-- | Construct 'DAWG' from the list of (word, value) pairs.
-- First a 'D.DAWG' is created and then it is frozen using
-- the 'freeze' function.
fromList :: (Enum a, Ord b) => [([a], b)] -> DAWG a () b
fromList = freeze . D.fromList
{-# SPECIALIZE fromList :: Ord b => [(String, b)] -> DAWG Char () b #-}


-- | Construct DAWG from the list of (word, value) pairs
-- with a combining function.  The combining function is
-- applied strictly. First a 'D.DAWG' is created and then
-- it is frozen using the 'freeze' function.
fromListWith :: (Enum a, Ord b) => (b -> b -> b) -> [([a], b)] -> DAWG a () b
fromListWith f = freeze . D.fromListWith f
{-# SPECIALIZE fromListWith
        :: Ord b => (b -> b -> b)
        -> [(String, b)] -> DAWG Char () b #-}


-- | Make DAWG from the list of words.  Annotate each word with
-- the @()@ value.  First a 'D.DAWG' is created and then it is frozen
-- using the 'freeze' function.
fromLang :: Enum a => [[a]] -> DAWG a () ()
fromLang = freeze . D.fromLang
{-# SPECIALIZE fromLang :: [String] -> DAWG Char () () #-}


-- | Weight of a node corresponds to the number of final states
-- reachable from the node.  Weight of an edge is a sum of weights
-- of preceding nodes outgoing from the same parent node.
type Weight = Int


-- | Compute node weights and store corresponding values in transition labels.
-- Be aware, that the entire DAWG will be weighted, even when (because of the use of
-- the `submap` function) only a part of the DAWG is currently selected.
weigh :: DAWG a b c -> DAWG a Weight c
weigh d = flip DAWG (root d) $ V.fromList
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
freeze :: D.DAWG a b -> DAWG a () b
freeze d = flip DAWG 0 . V.fromList $
    map (N.fromDyn newID . oldBy)
        (M.elems (inverse old2new))
  where
    -- Map from old to new identifiers.  The root identifier is mapped to 0.
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
-- thaw :: (Unbox b, Ord a) => DAWG a b c -> D.DAWG a b
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


-- | A number of distinct (key, value) pairs in the weighted DAWG.
size :: DAWG a Weight c -> Int
size d = size'I (root d) d


size'I :: ID -> DAWG a Weight c -> Int
size'I i d = add $ do
    x <- case N.edges n of
        [] -> Nothing
        xs -> Just (fst $ last xs)
    (j, v) <- N.onSym' x n
    return $ v + size'I j d
  where
    n = nodeBy i d
    u = maybe 0 (const 1) (leafValue n d)
    add m = u + maybe 0 id m


-- | Position in a set of all dictionary entries with respect
-- to the lexicographic order.
index :: Enum a => [a] -> DAWG a Weight c -> Maybe Int
index xs d = index'I (map fromEnum xs) (root d) d
{-# SPECIALIZE index :: String -> DAWG Char Weight c -> Maybe Int #-}


index'I :: [Sym] -> ID -> DAWG a Weight c -> Maybe Int
index'I []     i d = 0 <$ leafValue (nodeBy i d) d
index'I (x:xs) i d = do
    let n = nodeBy i d
        u = maybe 0 (const 1) (leafValue n d)
    (j, v) <- N.onSym' x n
    w <- index'I xs j d
    return (u + v + w)


-- | Find dictionary entry given its index with respect to the
-- lexicographic order.
byIndex :: Enum a => Int -> DAWG a Weight c -> Maybe [a]
byIndex ix d = map toEnum <$> byIndex'I ix (root d) d
{-# SPECIALIZE byIndex :: Int -> DAWG Char Weight c -> Maybe String #-}


byIndex'I :: Int -> ID -> DAWG a Weight c -> Maybe [Sym]
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
