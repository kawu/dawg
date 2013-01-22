{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | Internal representation of the "Data.DAWG" automaton.  Names in this
-- module correspond to a graphical representation of automaton: nodes refer
-- to states and edges refer to transitions.

module Data.DAWG.Graph
( Graph (..)
, empty
, nodes
, nodeBy
, insert
, delete
) where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
-- import Data.Binary (Binary, put, get)
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Control.Monad.ST

-- import Data.DAWG.HashMap (Hash)
-- import qualified Data.DAWG.HashMap as H
import Data.Hashable hiding (hash)
import qualified Data.HashTable.ST.Cuckoo as H

type ID = Int

-- | A set of nodes.  To every node a unique identifier is assigned.
-- Invariants: 
--
--   * freeIDs \\intersection occupiedIDs = \\emptySet,
--
--   * freeIDs \\sum occupiedIDs =
--     {0, 1, ..., |freeIDs \\sum occupiedIDs| - 1},
--
-- where occupiedIDs = elemSet idMap.
--
-- TODO: Is it possible to merge 'freeIDs' with 'ingoMap' to reduce
-- the memory footprint?
-- data Graph n = Graph {
--     -- | Map from nodes to IDs with hash values interpreted
--     -- as keys and (node, ID) pairs interpreted as map elements.
--       idMap     :: !(H.HashMap n ID)
--     -- | Set of free IDs.
--     , freeIDs   :: !S.IntSet
--     -- | Map from IDs to nodes. 
--     , nodeMap   :: !(M.IntMap n)
--     -- | Number of ingoing paths (different paths from the root
--     -- to the given node) for each node ID in the graph.
--     -- The number of ingoing paths can be also interpreted as
--     -- a number of occurences of the node in a tree representation
--     -- of the graph.
--     , ingoMap   :: !(M.IntMap Int) }
--     deriving (Show, Eq, Ord)

-- instance (Ord n, Binary n) => Binary (Graph n) where
--     put Graph{..} = do
--         put idMap
--         put freeIDs
--         put nodeMap
--         put ingoMap
--     get = Graph <$> get <*> get <*> get <*> get
-- 
-- -- | Empty graph.
-- empty :: Graph n
-- empty = Graph H.empty S.empty M.empty M.empty
-- 
-- -- | Size of the graph (number of nodes).
-- size :: Graph n -> Int
-- size = H.size . idMap
-- 
-- -- | List of graph nodes.
-- nodes :: Graph n -> [n]
-- nodes = M.elems . nodeMap
-- 
-- -- | Node with the given identifier.
-- nodeBy :: ID -> Graph n -> n
-- nodeBy i g = nodeMap g M.! i
-- 
-- -- | Retrieve identifier of a node assuming that the node
-- -- is present in the graph.  If the assumption is not
-- -- safisfied, the returned identifier may be incorrect.
-- nodeID'Unsafe :: Hash n => n -> Graph n -> ID
-- nodeID'Unsafe n g = H.lookupUnsafe n (idMap g)
-- 
-- -- | Add new graph node (assuming that it is not already a member
-- -- of the graph).
-- newNode :: Hash n => n -> Graph n -> (ID, Graph n)
-- newNode n Graph{..} =
--     (i, Graph idMap' freeIDs' nodeMap' ingoMap')
--   where
--     idMap'      = H.insertUnsafe n i idMap
--     nodeMap'    = M.insert i n nodeMap
--     ingoMap'    = M.insert i 1 ingoMap
--     (i, freeIDs') = if S.null freeIDs
--         then (H.size idMap, freeIDs)
--         else S.deleteFindMin freeIDs
-- 
-- -- | Remove node from the graph (assuming that it is a member
-- -- of the graph).
-- remNode :: Hash n => ID -> Graph n -> Graph n
-- remNode i Graph{..} =
--     Graph idMap' freeIDs' nodeMap' ingoMap'
--   where
--     idMap'      = H.deleteUnsafe n idMap
--     nodeMap'    = M.delete i nodeMap
--     ingoMap'    = M.delete i ingoMap
--     freeIDs'    = S.insert i freeIDs
--     n           = nodeMap M.! i
-- 
-- -- | Increment the number of ingoing paths.
-- incIngo :: ID -> Graph n -> Graph n
-- incIngo i g = g { ingoMap = M.insertWith' (+) i 1 (ingoMap g) }
-- 
-- -- | Decrement the number of ingoing paths and return
-- -- the resulting number.
-- decIngo :: ID -> Graph n -> (Int, Graph n)
-- decIngo i g =
--     let k = (ingoMap g M.! i) - 1
--     in  (k, g { ingoMap = M.insert i k (ingoMap g) })
-- 
-- -- | Insert node into the graph.  If the node was already a member
-- -- of the graph, just increase the number of ingoing paths.
-- -- NOTE: Number of ingoing paths will not be changed for any descendants
-- -- of the node, so the operation alone will not ensure that properties
-- -- of the graph are preserved.
-- insert :: Hash n => n -> Graph n -> (ID, Graph n)
-- insert n g = case H.lookup n (idMap g) of
--     Just i  -> (i, incIngo i g)
--     Nothing -> newNode n g
-- 
-- -- | Delete node from the graph.  If the node was present in the graph
-- -- at multiple positions, just decrease the number of ingoing paths.
-- -- Function crashes if the node is not a member of the graph. 
-- -- NOTE: The function does not delete descendant nodes which may become
-- -- inaccesible nor does it change the number of ingoing paths for any
-- -- descendant of the node.
-- delete :: Hash n => n -> Graph n -> Graph n
-- delete n g = if num == 0
--     then remNode i g'
--     else g'
--   where
--     i = nodeID'Unsafe n g
--     (num, g') = decIngo i g
 
data Graph s n = Graph {
    -- | Size of the graph (number of nodes).
      size      :: !Int
    -- | Map from nodes to IDs with hash values interpreted
    -- as keys and (node, ID) pairs interpreted as map elements.
    , idMap     :: !(H.HashTable s n ID)
    -- | Set of free IDs.
    , freeIDs   :: !S.IntSet
    -- | Map from IDs to nodes. 
    , nodeMap   :: !(M.IntMap n)
    -- | Number of ingoing paths (different paths from the root
    -- to the given node) for each node ID in the graph.
    -- The number of ingoing paths can be also interpreted as
    -- a number of occurences of the node in a tree representation
    -- of the graph.
    , ingoMap   :: !(M.IntMap Int) }
    deriving (Show)

-- | Empty graph.
empty :: ST s (Graph s n)
empty = do
    ht <- H.new
    return $ Graph 0 ht S.empty M.empty M.empty

-- | List of graph nodes.
nodes :: Graph s n -> [n]
nodes = M.elems . nodeMap

-- | Node with the given identifier.
nodeBy :: ID -> Graph s n -> n
nodeBy i g = nodeMap g M.! i

-- | Retrieve identifier of a node assuming that the node
-- is present in the graph.  If the assumption is not
-- safisfied, the returned identifier may be incorrect.
-- TODO: Use lookupUnsafe.
nodeID'Unsafe :: (Eq n, Hashable n) => n -> Graph s n -> ST s ID
nodeID'Unsafe n g = fromJust <$> H.lookup (idMap g) n

-- | Add new graph node (assuming that it is not already a member
-- of the graph).
-- TODO: Use insertUnsafe for idMap.
newNode :: (Eq n, Hashable n) => n -> Graph s n -> ST s (ID, Graph s n)
newNode n Graph{..} = do
    H.insert idMap n i
    let graph = Graph (size+1) idMap freeIDs' nodeMap' ingoMap'
    return $ (i, graph)
  where
    -- idMap'   = H.insertUnsafe n i idMap
    nodeMap'    = M.insert i n nodeMap
    ingoMap'    = M.insert i 1 ingoMap
    (i, freeIDs') = if S.null freeIDs
        then (size, freeIDs)
        else S.deleteFindMin freeIDs

-- | Remove node from the graph (assuming that it is a member
-- of the graph).
-- TODO: Use deleteUnsafe for idMap.
remNode :: (Eq n, Hashable n) => ID -> Graph s n -> ST s (Graph s n)
remNode i Graph{..} = do
    H.delete idMap n
    return $ Graph (size-1) idMap freeIDs' nodeMap' ingoMap'
  where
    -- idMap'      = H.deleteUnsafe n idMap
    nodeMap'    = M.delete i nodeMap
    ingoMap'    = M.delete i ingoMap
    freeIDs'    = S.insert i freeIDs
    n           = nodeMap M.! i

-- | Increment the number of ingoing paths.
incIngo :: ID -> Graph s n -> Graph s n
incIngo i g = g { ingoMap = M.insertWith' (+) i 1 (ingoMap g) }

-- | Decrement the number of ingoing paths and return
-- the resulting number.
decIngo :: ID -> Graph s n -> (Int, Graph s n)
decIngo i g =
    let k = (ingoMap g M.! i) - 1
    in  (k, g { ingoMap = M.insert i k (ingoMap g) })

-- | Insert node into the graph.  If the node was already a member
-- of the graph, just increase the number of ingoing paths.
-- NOTE: Number of ingoing paths will not be changed for any descendants
-- of the node, so the operation alone will not ensure that properties
-- of the graph are preserved.
insert :: (Eq n, Hashable n) => n -> Graph s n -> ST s (ID, Graph s n)
insert n g =
    H.lookup (idMap g) n >>= \mi -> case mi of
        Just i  -> return (i, incIngo i g)
        Nothing -> newNode n g

-- | Delete node from the graph.  If the node was present in the graph
-- at multiple positions, just decrease the number of ingoing paths.
-- Function crashes if the node is not a member of the graph. 
-- NOTE: The function does not delete descendant nodes which may become
-- inaccesible nor does it change the number of ingoing paths for any
-- descendant of the node.
delete :: (Eq n, Hashable n) => n -> Graph s n -> ST s (Graph s n)
delete n g = do
    i <- nodeID'Unsafe n g
    let (num, g') = decIngo i g
    if num == 0
        then remNode i g'
        else return g'

-- -- | Construct a graph from a list of node/ID pairs and a root ID.
-- -- Identifiers must be consistent with edges outgoing from
-- -- individual nodes.
-- fromNodes :: Ord a => [(Node a, ID)] -> ID -> Graph a
-- fromNodes xs rootID = graph
--   where
--     graph = Graph
--         (M.fromList xs)
--         IS.empty
--         (IM.fromList $ map swap xs)
--         ( foldl' updIngo (IM.singleton rootID 1)
--             $ topSort graph rootID )
--     swap (x, y) = (y, x)
--     updIngo m i =
--         let n = nodeBy i graph
--             ingo = m IM.! i
--         in  foldl' (push ingo) m (edges n)
--     push x m j = IM.adjust (+x) j m
-- 
-- postorder :: T.Tree a -> [a] -> [a]
-- postorder (T.Node a ts) = postorderF ts . (a :)
-- 
-- postorderF :: T.Forest a -> [a] -> [a]
-- postorderF ts = foldr (.) id $ map postorder ts
-- 
-- postOrd :: Graph a -> ID -> [ID]
-- postOrd g i = postorder (dfs g i) []
-- 
-- -- | Topological sort given a root ID.
-- topSort :: Graph a -> ID -> [ID]
-- topSort g = reverse . postOrd g
-- 
-- -- | Depth first search starting with given ID.
-- dfs :: Graph a -> ID -> T.Tree ID
-- dfs g = prune . generate g
-- 
-- generate :: Graph a -> ID -> T.Tree ID
-- generate g i = T.Node i
--     ( T.Node (eps n) []
--     : map (generate g) (edges n) )
--   where
--     n = nodeBy i g
-- 
-- type SetM a = S.State IS.IntSet a
-- 
-- run :: SetM a -> a
-- run act = S.evalState act IS.empty
-- 
-- contains :: ID -> SetM Bool
-- contains i = IS.member i <$> S.get
-- 
-- include :: ID -> SetM ()
-- include i = S.modify (IS.insert i)
-- 
-- prune :: T.Tree ID -> T.Tree ID
-- prune t = head $ run (chop [t])
-- 
-- chop :: T.Forest ID -> SetM (T.Forest ID)
-- chop [] = return []
-- chop (T.Node v ts : us) = do
--     visited <- contains v
--     if visited then
--         chop us
--     else do
--         include v
--         as <- chop ts
--         bs <- chop us
--         return (T.Node v as : bs)
