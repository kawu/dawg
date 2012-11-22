{-# LANGUAGE RecordWildCards #-}

-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
--
-- In comparison to "Data.DAWG" module the automaton implemented here:
--
--   * Keeps all nodes in one array and thus uses much less memory,
--
--   * Constitutes a /perfect hash automaton/ which provides
--     'hash' and 'unHash' functions,
--
--   * Doesn't provide insert/delete family of operations.
--
-- Use the 'freeze' and 'thaw' functions to convert between both DAWG
-- representations.

module Data.DAWG.Frozen
( Node (..)
, DAWG
, empty
, lookup
, freeze
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Data.Binary (Binary, put, get)
import Data.Vector.Binary ()
import qualified Data.IntMap as M
import qualified Data.Vector as V

import qualified Data.DAWG.VMap as VM
import qualified Data.DAWG.Internal as I
import qualified Data.DAWG as D

-- | Node identifier.
type Id = Int

-- | State (node) of the automaton.
data Node a = Node
    { value :: !a
    , edges :: !VM.VMap }
    deriving (Show, Eq, Ord)

instance Binary a => Binary (Node a) where
    put Node{..} = put value >> put edges
    get = Node <$> get <*> get

-- | Identifier of the child determined by the given symbol.
onSym :: Int -> Node a -> Maybe Id
onSym x (Node _ es) = VM.lookup x es

-- | Root is stored on the first position of the array.
type DAWG a b = V.Vector (Node (Maybe b))

-- | Empty DAWG.
empty :: DAWG a b
empty = V.singleton (Node Nothing VM.empty)

-- | Node with the given identifier.
nodeBy :: Id -> DAWG a b -> Node (Maybe b)
nodeBy i d = d V.! i

lookup'I :: [Int] -> Id -> DAWG a b -> Maybe b
lookup'I []     i d = value (nodeBy i d)
lookup'I (x:xs) i d = case onSym x (nodeBy i d) of
    Just j  -> lookup'I xs j d
    Nothing -> Nothing

-- | Find value associated with the key.
lookup :: Enum a => [a] -> DAWG a b -> Maybe b
lookup xs' d =
    let xs = map fromEnum xs'
    in  lookup'I xs 0 d
{-# SPECIALIZE lookup :: String -> DAWG Char b -> Maybe b #-}

-- | Freeze the "D.DAWG" version of automaton.
freeze :: D.DAWG a b -> DAWG a b
freeze d = V.fromList $
    map (stop . oldBy) (M.elems (inverse old2new))
  where
    -- Map from old to new identifiers.
    old2new = M.fromList $ (D.root d, 0) : zip (nodeIDs d) [1..]
    -- List of non-frozen branches' IDs without the root ID.
    nodeIDs = filter (/= D.root d) . branchIDs
    -- Make frozen node with new IDs from non-frozen node.
    stop    = Node <$> onEps <*> mkEdges . I.edgeMap
    -- Extract value following the epsilon transition.
    onEps   = I.unValue . oldBy . I.eps
    -- List of edges with new IDs.
    mkEdges = VM.fromList . map (second (old2new M.!)) . VM.toList 
    -- Non-frozen node by given identifier.
    oldBy i = I.nodeBy i (D.graph d)

-- | Branch IDs in the non-frozen DAWG.
branchIDs :: D.DAWG a b -> [I.Id]
branchIDs
    = map fst . filter (isBranch . snd)
    . M.assocs . I.nodeMap . D.graph
  where
    isBranch (I.Branch _ _) = True
    isBranch _              = False
        
-- | Inverse of the map.
inverse :: M.IntMap Int -> M.IntMap Int
inverse =
    let swap (x, y) = (y, x)
    in  M.fromList . map swap . M.toList
