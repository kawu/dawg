{-# LANGUAGE RecordWildCards #-}

-- | The module provides a /directed acyclic graph/ (DAG) implementation
-- where all equivalent nodes (i.e. roots of DAGs equal with respect
-- to '==') are compressed to one node with unique identifier.
-- It can be alternatively thought of as a /minimal acyclic
-- finite-state automata/.

module Data.DAWG.Graph
( 
-- * Node
  Node (..)
, Id
, leaf
, onChar
, subst
-- * Graph
, Graph (..)
, empty
, size
, nodeBy
, nodeID
, insert
, delete
) where

import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

import qualified Data.DAWG.VMap as V

-- | Node identifier.
type Id = Int

-- | Two nodes (states) belong to the same equivalence class (and,
-- consequently, they must be represented as one node in the graph)
-- iff they are equal with respect to their values and outgoing
-- edges.
data Node a = Node
    { value :: Maybe a
    , edges :: V.VMap Id }
    deriving (Show, Eq, Ord)

-- | Leaf node with no children and 'Nothing' value.
leaf :: Node a
leaf = Node
    { value = Nothing
    , edges = V.empty }

-- | Child identifier found by following the given character.
onChar :: Char -> Node a -> Maybe Id
onChar x n = V.lookup x (edges n)

-- | Substitue the identifier of the child determined by the given
-- character.
subst :: Char -> Id -> Node a -> Node a
subst x i n = n { edges = V.insert x i (edges n) }

-- | A set of nodes.  To every node a unique identifier is assigned.
-- Invariants: (1) freeIDs \intersection elemSet idMap = \emptySet
-- (2) freeIDs \sum elemSet idMap = {0, 1, ..., maximum (elemSet idMap) - 1}.
-- TODO: Is it possible to enforce that idMap keys are nodeMap elems
-- are not duplicated in memory?
-- TODO: Is it possible to merge freeIDs with ingoMap to save some memory?
data Graph a = Graph {
    -- | Map from nodes to IDs.
      idMap     :: !(M.Map (Node a) Id)
    -- | Set of free IDs.
    , freeIDs   :: !IS.IntSet
    -- | Equivalence class represented by given ID and size of the class. 
    , nodeMap   :: !(IM.IntMap (Node a))
    -- | Number of ingoing edges.
    , ingoMap   :: !(IM.IntMap Int) }
    deriving (Show, Eq, Ord)

-- | Empty graph.
empty :: Graph a
empty = Graph
    (M.singleton leaf 0)
    IS.empty
    (IM.singleton 0 leaf)
    (IM.singleton 0 1)

-- | Size of the graph (number of nodes).
size :: Graph a -> Int
size = M.size . idMap

-- | Node with the given identifier.
nodeBy :: Id -> Graph a -> Node a
nodeBy i g = nodeMap g IM.! i

-- | Retrive the node identifier.
nodeID :: Ord a => Node a -> Graph a -> Id
nodeID n g = idMap g M.! n

-- | Add new graph node.
newNode :: Ord a => Node a -> Graph a -> (Id, Graph a)
newNode n Graph{..} =
    (i, Graph idMap' freeIDs' nodeMap' ingoMap')
  where
    idMap'      = M.insert  n i idMap
    nodeMap'    = IM.insert i n nodeMap
    ingoMap'    = IM.insert i 1 ingoMap
    (i, freeIDs') = if IS.null freeIDs
        then (M.size idMap, freeIDs)
        else IS.deleteFindMin freeIDs

-- | Remove node from the graph.
remNode :: Ord a => Id -> Graph a -> Graph a
remNode i Graph{..} =
    Graph idMap' freeIDs' nodeMap' ingoMap'
  where
    idMap'      = M.delete  n idMap
    nodeMap'    = IM.delete i nodeMap
    ingoMap'    = IM.delete i ingoMap
    freeIDs'    = IS.insert i freeIDs
    n           = nodeMap IM.! i

-- | Increment the number of ingoing edges.
incIngo :: Id -> Graph a -> Graph a
incIngo i g = g { ingoMap = IM.adjust (+1) i (ingoMap g) }

-- | Descrement the number of ingoing edges and return
-- the resulting number.
decIngo :: Id -> Graph a -> (Int, Graph a)
decIngo i g =
    let k = (ingoMap g IM.! i) - 1
    in  (k, g { ingoMap = IM.insert i k (ingoMap g) })

-- | Insert node into the graph.  If the node was already a member
-- of the graph, just increase the number of ingoing edges.
insert :: Ord a => Node a -> Graph a -> (Id, Graph a)
insert n g = case M.lookup n (idMap g) of
    Just i  -> (i, incIngo i g)
    Nothing -> newNode n g

-- | Delete node from the graph.  If the node was present in the graph
-- on multiple positions, just decrease the number of ingoing edges.
delete :: Ord a => Node a -> Graph a -> Graph a
delete n g = if num == 0
    then remNode i g'
    else g'
  where
    i = nodeID n g
    (num, g') = decIngo i g
