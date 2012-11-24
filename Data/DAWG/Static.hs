{-# LANGUAGE RecordWildCards #-}

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
  DAWG (..)
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
import Control.Applicative ((<$), (<$>), (<*>), (<|>))
import Control.Arrow (first, second)
import Data.Binary (Binary, Get, put, get)
import Data.Vector.Binary ()
import Data.Vector.Unboxed (Unbox)
import qualified Data.IntMap as M
import qualified Data.Vector as V

import qualified Data.DAWG.VMap as VM
import qualified Data.DAWG.Internal as I
import qualified Data.DAWG as D

-- | Node identifier.
type ID = Int

-- | Internal representation of the transition symbol.
type Sym = Int

-- | Edge with label.
type Edge a = (ID, a)

to :: Edge a -> ID
to = fst
{-# INLINE to #-}

label :: Edge a -> a
label = snd
{-# INLINE label #-}

annotate :: a -> Edge b -> Edge a
annotate x (i, _) = (i, x)
{-# INLINE annotate #-}

labeled :: a -> ID -> Edge a
labeled x i = (i, x)
{-# INLINE labeled #-}

-- | State (node) of the automaton.
data Node a b 
    = Branch {
        -- | Epsilon transition.
          eps       :: {-# UNPACK #-} !ID
        -- | Labeled edges outgoing from the node.
        , edgeMap   :: !(VM.VMap (Edge b)) }
    | Leaf { value  :: !a }
    deriving (Show, Eq, Ord)

instance (Unbox b, Binary a, Binary b) => Binary (Node a b) where
    put Branch{..} = put (1 :: Int) >> put eps >> put edgeMap
    put Leaf{..}   = put (2 :: Int) >> put value
    get = do
        x <- get :: Get Int
        case x of
            1 -> Branch <$> get <*> get
            _ -> Leaf <$> get

-- instance (Unbox b, Binary a, Binary b) => Binary (Node a b) where
--     put Node{..} = put value >> put edgeMap
--     get = Node <$> get <*> get

-- | Transition function.
onSym :: Unbox b => Sym -> Node a b -> Maybe (Edge b)
onSym x (Branch _ es)   = VM.lookup x es
onSym _ (Leaf _)        = Nothing
{-# INLINE onSym #-}

-- List of symbol/edge pairs outgoing from the node.
trans :: Unbox b => Node a b -> [(Sym, Edge b)]
trans (Branch _ es)     = VM.toList es
trans (Leaf _)          = []
{-# INLINE trans #-}

-- | List of outgoing edges.
edges :: Unbox b => Node a b -> [Edge b]
edges = map snd . trans
{-# INLINE edges #-}

-- | List children identifiers.
children :: Unbox b => Node a b -> [ID]
children = map to . edges
{-# INLINE children #-}

-- | @DAWG a b c@ constitutes an automaton with alphabet symbols of type /a/,
-- node values of type /Maybe b/ and additional transition labels of type /c/.
-- Root is stored on the first position of the array.
newtype DAWG a b c = DAWG { unDAWG :: V.Vector (Node (Maybe b) c) }

-- | Empty DAWG.
empty :: Unbox c => DAWG a b c
empty = DAWG $ V.fromList
    [ Branch 1 VM.empty
    , Leaf Nothing ]

-- | Number of states in the automaton.
numStates :: DAWG a b c -> Int
numStates = V.length . unDAWG

-- | Node with the given identifier.
nodeBy :: ID -> DAWG a b c -> Node (Maybe b) c
nodeBy i d = unDAWG d V.! i

-- | Value in leaf node with a given ID.
leafValue :: Node (Maybe b) c -> DAWG a b c -> Maybe b
leafValue n = value . nodeBy (eps n)

-- | Find value associated with the key.
lookup :: (Unbox c, Enum a) => [a] -> DAWG a b c -> Maybe b
lookup xs' =
    let xs = map fromEnum xs'
    in  lookup'I xs 0
{-# SPECIALIZE lookup :: Unbox c => String -> DAWG Char b c -> Maybe b #-}

lookup'I :: Unbox c => [Sym] -> ID -> DAWG a b c -> Maybe b
lookup'I []     i d = leafValue (nodeBy i d) d
lookup'I (x:xs) i d = case onSym x (nodeBy i d) of
    Just e  -> lookup'I xs (to e) d
    Nothing -> Nothing

-- | Return all key/value pairs in the DAWG in ascending key order.
assocs :: (Enum a, Unbox c) => DAWG a b c -> [([a], b)]
assocs d = map (first (map toEnum)) (assocs'I 0 d)
{-# SPECIALIZE assocs :: Unbox c => DAWG Char b c -> [(String, b)] #-}

assocs'I :: Unbox c => ID -> DAWG a b c -> [([Sym], b)]
assocs'I i d =
    here ++ concatMap there (trans n)
  where
    n = nodeBy i d
    here = case leafValue n d of
        Just x  -> [([], x)]
        Nothing -> []
    there (x, e) = map (first (x:)) (assocs'I (to e) d)

-- | Return all keys of the DAWG in ascending order.
keys :: (Unbox c, Enum a) => DAWG a b c -> [[a]]
keys = map fst . assocs
{-# SPECIALIZE keys :: Unbox c => DAWG Char b c -> [String] #-}

-- | Return all elements of the DAWG in the ascending order of their keys.
elems :: Unbox c => DAWG a b c -> [b]
elems = map snd . assocs'I 0

-- | Construct 'DAWG' from the list of (word, value) pairs.
-- First a 'D.DAWG' is created and then it is frozen using
-- the 'freeze' function.
fromList :: (Enum a, Ord b) => [([a], b)] -> DAWG a b ()
fromList = freeze . D.fromList
{-# SPECIALIZE fromList :: Ord b => [(String, b)] -> DAWG Char b () #-}

-- | Construct DAWG from the list of (word, value) pairs
-- with a combining function.  The combining function is
-- applied strictly. First a 'D.DAWG' is created and then
-- it is frozen using the 'freeze' function.
fromListWith :: (Enum a, Ord b) => (b -> b -> b) -> [([a], b)] -> DAWG a b ()
fromListWith f = freeze . D.fromListWith f
{-# SPECIALIZE fromListWith :: Ord b => (b -> b -> b)
        -> [(String, b)] -> DAWG Char b () #-}

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
weigh :: Unbox c => DAWG a b c -> DAWG a b Weight
weigh d = (DAWG . V.fromList)
    [ branch n (apply ws (trans n))
    | i <- [0 .. numStates d - 1]
    , let n  = nodeBy i d
    , let ws = accum (children n) ]
  where
    -- Branch with new edges.
    branch Branch{..} es    = Branch eps es
    branch Leaf{..}   _     = Leaf value
    -- In nodeWeight node weights are memoized.
    nodeWeight = ((V.!) . V.fromList) (map detWeight [0 .. numStates d - 1])
    -- Determine weight of the node.
    detWeight i = case nodeBy i d of
        Leaf w  -> maybe 0 (const 1) w
        n       -> sum . map nodeWeight $ allChildren n
    -- Weight for subsequent edges.
    accum = init . scanl (+) 0 . map nodeWeight
    -- Apply weight to edges. 
    apply ws ts = VM.fromList
        [ (x, annotate w e)
        | (w, (x, e)) <- zip ws ts ]
    -- Plain children and epsilon child. 
    allChildren n = eps n : children n

-- | Construct immutable version of the automaton.
freeze :: D.DAWG a b -> DAWG a b ()
freeze d = DAWG . V.fromList $
    map (mkNode . oldBy) (M.elems (inverse old2new))
  where
    -- Map from old to new identifiers.
    old2new = M.fromList $ (D.root d, 0) : zip (nodeIDs d) [1..]
    mkID    = (M.!) old2new
    -- List of node IDs without the root ID.
    nodeIDs = filter (/= D.root d) . map fst . M.assocs . I.nodeMap . D.graph
    -- Make node from the old one.
    mkNode I.Branch{..} = Branch (mkID eps) (mkEdges edgeMap)
    mkNode I.Leaf{..}   = Leaf value
    -- List of edges with new IDs.
    mkEdges = VM.fromList . map (second mkEdge) . VM.toList
    -- Make edge from old ID.
    mkEdge = labeled () . mkID
    -- Non-frozen node by given identifier.
    oldBy i = I.nodeBy i (D.graph d)
        
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
index :: Enum a => [a] -> DAWG a b Weight -> Maybe Int
index xs = index'I (map fromEnum xs) 0
{-# SPECIALIZE index :: String -> DAWG Char b Weight -> Maybe Int #-}

index'I :: [Sym] -> ID -> DAWG a b Weight -> Maybe Int
index'I []     i d = 0 <$ leafValue (nodeBy i d) d
index'I (x:xs) i d = do
    let n = nodeBy i d
        v = maybe 0 (const 1) (leafValue n d)
    e <- onSym x n
    w <- index'I xs (to e) d
    return (v + w + label e)

-- | Perfect hashing function for dictionary entries.
-- A synonym for the 'index' function.
hash :: Enum a => [a] -> DAWG a b Weight -> Maybe Int
hash = index
{-# INLINE hash #-}

-- | Find dictionary entry given its index with respect to the
-- lexicographic order.
byIndex :: Enum a => Int -> DAWG a b Weight -> Maybe [a]
byIndex ix d = map toEnum <$> byIndex'I ix 0 d
{-# SPECIALIZE byIndex :: Int -> DAWG Char b Weight -> Maybe String #-}

byIndex'I :: Int -> ID -> DAWG a b Weight -> Maybe [Sym]
byIndex'I ix i d
    | ix < 0    = Nothing
    | otherwise = here <|> there
  where
    n = nodeBy i d
    v = maybe 0 (const 1) (leafValue n d)
    here
        | ix == 0   = [] <$ leafValue (nodeBy i d) d
        | otherwise = Nothing
    there = do
        (x, e) <- VM.findLastLE cmp (edgeMap n)
        xs <- byIndex'I (ix - v - label e) (to e) d
        return (x:xs)
    cmp e = compare (label e) (ix - v)

-- | Inverse of the 'hash' function and a synonym for the 'byIndex' function.
unHash :: Enum a => Int -> DAWG a b Weight -> Maybe [a]
unHash = byIndex
{-# INLINE unHash #-}
