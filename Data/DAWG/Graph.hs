{-# LANGUAGE RecordWildCards #-}

-- | The module provides a representation of a tree where all equivalent nodes
-- (i.e. trees equal with respect to the '==' function) are compressed to one
-- /directed acyclic graph/ (DAG) node with unique identifier.  Alternatively,
-- it can be thought of as a /minimal acyclic finite-state automata/.

module Data.DAWG.Graph
( 
-- * Node
  Node (..)
, Id
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

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, put, get)
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

import qualified Data.DAWG.VMap as V

-- | Node identifier.
type Id = Int

-- | Two nodes (states) belong to the same equivalence class (and,
-- consequently, they must be represented as one node in the graph)
-- iff they are equal with respect to their values and outgoing
-- edges.
--
-- Invariant: the 'value' identifier always points to the 'Value' node.
-- 'edges', on the other hand, point to 'Branch' nodes.
data Node a
    = Branch
        { eps   :: {-# UNPACK #-} !Id
        , edges :: !(V.VMap Id) }
    | Value
        { unValue :: !a }
    deriving (Show, Eq, Ord)

-- instance Functor Node where
--     fmap f n = n { value = f (value n) }
-- 
-- instance Binary a => Binary (Node a) where
--     put Node{..} = put value >> put edges
--     get = Node <$> get <*> get

-- | Identifier of the child determined by the given character.
onChar :: Char -> Node a -> Maybe Id
onChar x (Branch _ es)  = V.lookup x es
onChar _ (Value _)      = error "onChar: value node"

-- | Substitue the identifier of the child determined by the given
-- character.
subst :: Char -> Id -> Node a -> Node a
subst x i (Branch w es) = Branch w (V.insert x i es)
subst _ _ (Value _)     = error "subst: value node"

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
-- TODO: Is it possible to merge freeIDs with ingoMap to save some memory?
data Graph a = Graph {
    -- | Map from nodes to IDs.
      idMap     :: !(M.Map (Node a) Id)
    -- | Set of free IDs.
    , freeIDs   :: !IS.IntSet
    -- | Map from IDs to nodes. 
    , nodeMap   :: !(IM.IntMap (Node a))
    -- | Number of ingoing paths (different paths from the root
    -- to the given node) for each node ID in the graph.
    -- The number of ingoing paths can be also interpreted as
    -- a number of occurences of the node in a tree representation
    -- of the graph.
    , ingoMap   :: !(IM.IntMap Int) }
    deriving (Show, Eq, Ord)

-- instance (Ord a, Binary a) => Binary (Graph a) where
--     put Graph{..} = do
--     	put idMap
-- 	put freeIDs
-- 	put nodeMap
-- 	put ingoMap
--     get = Graph <$> get <*> get <*> get <*> get

-- | Empty graph.
empty :: Graph a
empty = Graph M.empty IS.empty IM.empty IM.empty

-- | Size of the graph (number of nodes).
size :: Graph a -> Int
size = M.size . idMap

-- | Node with the given identifier.
nodeBy :: Id -> Graph a -> Node a
nodeBy i g = nodeMap g IM.! i

-- | Retrieve the node identifier.
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

-- | Increment the number of ingoing paths.
incIngo :: Id -> Graph a -> Graph a
incIngo i g = g { ingoMap = IM.insertWith' (+) i 1 (ingoMap g) }

-- | Descrement the number of ingoing paths and return
-- the resulting number.
decIngo :: Id -> Graph a -> (Int, Graph a)
decIngo i g =
    let k = (ingoMap g IM.! i) - 1
    in  (k, g { ingoMap = IM.insert i k (ingoMap g) })

-- | Insert node into the graph.  If the node was already a member
-- of the graph, just increase the number of ingoing paths.
-- NOTE: Number of ingoing paths will not be changed for any
-- ancestors of the node, so the operation alone will not ensure
-- that properties of the graph are preserved.
insert :: Ord a => Node a -> Graph a -> (Id, Graph a)
insert n g = case M.lookup n (idMap g) of
    Just i  -> (i, incIngo i g)
    Nothing -> newNode n g

-- | Delete node from the graph.  If the node was present in the graph
-- at multiple positions, just decrease the number of ingoing paths.
-- NOTE: The function does not delete descendant nodes which may become
-- inaccesible nor does it change the number of ingoing paths for any
-- ancestor of the node.
delete :: Ord a => Node a -> Graph a -> Graph a
delete n g = if num == 0
    then remNode i g'
    else g'
  where
    i = nodeID n g
    (num, g') = decIngo i g
