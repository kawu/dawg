{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | Internal representation of the "Data.DAWG" automaton.  Names in this
-- module correspond to a graphical representation of automaton: nodes refer
-- to states and edges refer to transitions.

module Data.DAWG.Internal
( Graph (..)
, empty
, size
, nodeBy
, nodeID
, insert
, delete
) where

import Control.Applicative ((<$>), (<*>))
-- import Data.List (foldl')
import Data.Binary (Binary, put, get)
import qualified Data.Map as M
-- import qualified Data.Tree as T
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
-- import qualified Control.Monad.State.Strict as S

import Data.DAWG.Node.Specialized hiding (Node)
import qualified Data.DAWG.Node.Specialized as N

type Node a = N.Node a

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
data Graph a = Graph {
    -- | Map from nodes to IDs.
      idMap     :: !(M.Map (Node a) ID)
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

instance (Ord a, Binary a) => Binary (Graph a) where
    put Graph{..} = do
    	put idMap
	put freeIDs
	put nodeMap
	put ingoMap
    get = Graph <$> get <*> get <*> get <*> get

-- | Empty graph.
empty :: Graph a
empty = Graph M.empty IS.empty IM.empty IM.empty

-- | Size of the graph (number of nodes).
size :: Graph a -> Int
size = M.size . idMap

-- | Node with the given identifier.
nodeBy :: ID -> Graph a -> Node a
nodeBy i g = nodeMap g IM.! i

-- | Retrieve the node identifier.
nodeID :: Ord a => Node a -> Graph a -> ID
nodeID n g = idMap g M.! n

-- | Add new graph node.
newNode :: Ord a => Node a -> Graph a -> (ID, Graph a)
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
remNode :: Ord a => ID -> Graph a -> Graph a
remNode i Graph{..} =
    Graph idMap' freeIDs' nodeMap' ingoMap'
  where
    idMap'      = M.delete  n idMap
    nodeMap'    = IM.delete i nodeMap
    ingoMap'    = IM.delete i ingoMap
    freeIDs'    = IS.insert i freeIDs
    n           = nodeMap IM.! i

-- | Increment the number of ingoing paths.
incIngo :: ID -> Graph a -> Graph a
incIngo i g = g { ingoMap = IM.insertWith' (+) i 1 (ingoMap g) }

-- | Decrement the number of ingoing paths and return
-- the resulting number.
decIngo :: ID -> Graph a -> (Int, Graph a)
decIngo i g =
    let k = (ingoMap g IM.! i) - 1
    in  (k, g { ingoMap = IM.insert i k (ingoMap g) })

-- | Insert node into the graph.  If the node was already a member
-- of the graph, just increase the number of ingoing paths.
-- NOTE: Number of ingoing paths will not be changed for any descendants
-- of the node, so the operation alone will not ensure that properties
-- of the graph are preserved.
insert :: Ord a => Node a -> Graph a -> (ID, Graph a)
insert n g = case M.lookup n (idMap g) of
    Just i  -> (i, incIngo i g)
    Nothing -> newNode n g

-- | Delete node from the graph.  If the node was present in the graph
-- at multiple positions, just decrease the number of ingoing paths.
-- NOTE: The function does not delete descendant nodes which may become
-- inaccesible nor does it change the number of ingoing paths for any
-- descendant of the node.
delete :: Ord a => Node a -> Graph a -> Graph a
delete n g = if num == 0
    then remNode i g'
    else g'
  where
    i = nodeID n g
    (num, g') = decIngo i g

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
