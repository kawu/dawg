{-# LANGUAGE RecordWildCards #-}

-- | The module provides implementation of /directed acyclic word graphs/
-- (DAWGs) also known as /minimal acyclic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.

module Data.DAWG
( Id
, Node (..)
, Graph (..)
, emptyG
, sizeG
, DAWG (..)
, empty
, size
, insert
, lookup
, fromList
, fromLang
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Control.Monad.State.Strict as S

import qualified Data.DAWG.VMap as V

-- | Node identifier.
type Id = Int

-- | Two nodes (states) belong to the same equivalence class (and,
-- consequently, they myst be represented as one node in the DAWG)
-- iff they are equal with respect to their values and outgoing
-- edges.
data Node a = Node
    { value :: Maybe a
    , edges :: V.VMap Id }
    deriving (Show, Eq, Ord)

leaf :: Node a
leaf = Node
    { value = Nothing
    , edges = V.empty }

-- | A set of nodes.  To every node a unique identifier is assigned.
-- Invariant: freeIDs \intersection elemSet idMap = \emptySet and freeIDs
-- \sum elemSet idMap = {0, 1, ..., maximum (elemSet idMap) - 1}.
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
emptyG :: Graph a
emptyG = Graph
    (M.singleton leaf 0)
    IS.empty
    (IM.singleton 0 leaf)
    (IM.singleton 0 1)

-- | Size of the graph (number of nodes).
sizeG :: Graph a -> Int
sizeG = M.size . idMap

-- | Add new graph node.
newNode :: Ord a => Node a -> Graph a -> (Id, Graph a)
newNode n Graph{..} =
    (i, Graph idMap' freeIDs' nodeMap' ingoMap')
  where
    idMap'      = M.insert  n i idMap
    nodeMap'    = IM.insert i n nodeMap
    ingoMap'    = IM.insert i 0 ingoMap
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

type GraphM a b = S.State (Graph a) b

mkState :: (Graph a -> Graph a) -> Graph a -> ((), Graph a)
mkState f g = ((), f g)

-- | Return node with the given identifier.
nodeBy :: Id -> GraphM a (Node a)
nodeBy i = do
    m <- nodeMap <$> S.get 
    return (m IM.! i)

idBy :: Ord a => Node a -> GraphM a Id
idBy n = do
    m <- idMap <$> S.get
    return (m M.! n)

-- | Child identifier found by following the given character.
onChar :: Char -> Node a -> Maybe Id
onChar x n = V.lookup x (edges n)

-- | Increase the number of node ingoing edges and, if the node
-- is not a member of the graph, add it.  Return the node identifier
-- resulting from the operation.
hoist :: Ord a => Node a -> GraphM a Id
hoist n = do
    m <- idMap <$> S.get
    i <- case M.lookup n m of
        Just i  -> return i
        Nothing -> S.state (newNode n)
    S.state (mkState (incIngo i))
    return i

-- | Decrease number of node ingoing edges and, if the resulting number
-- is equal to 0, remove the node from the graph.
lower :: Ord a => Node a -> GraphM a ()
lower n = do
    i <- idBy n
    num <- S.state (decIngo i)
    when (num == 0) $ do
        S.state (mkState (remNode i))

-- | Substitue the identifier of the child determined by the given
-- character.
subst :: Char -> Id -> Node a -> Node a
subst x i n = n { edges = V.insert x i (edges n) }
{-# INLINE subst #-}

insertM :: Ord a => String -> a -> Id -> GraphM a Id
insertM [] y i = do
    n <- nodeBy i
    lower n
    hoist (n { value = Just y })
insertM (x:xs) y i = do
    n <- nodeBy i
    j <- case onChar x n of
        Just j  -> return j
        Nothing -> hoist leaf
    k <- insertM xs y j
    lower n
    hoist (subst x k n)
    
lookupM :: String -> Id -> GraphM a (Maybe a)
lookupM [] i = value <$> nodeBy i
lookupM (x:xs) i = do
    n <- nodeBy i
    case onChar x n of
        Just j  -> lookupM xs j
        Nothing -> return Nothing

data DAWG a = DAWG
    { graph :: !(Graph a)
    , root  :: !Id }
    deriving (Show, Eq, Ord)

-- | Empty DAWG.
empty :: DAWG a
empty = DAWG emptyG 0

-- | DAWG size (number of nodes).
size :: DAWG a -> Int
size = sizeG . graph

insert :: Ord a => String -> a -> DAWG a -> DAWG a
insert xs y d =
    let (i, g) = S.runState (insertM xs y $ root d) (graph d)
    in  DAWG g i

lookup :: String -> DAWG a -> Maybe a
lookup xs d = S.evalState (lookupM xs $ root d) (graph d)

-- | Construct DAWG from the list of (word, value) pairs.
fromList :: (Ord a) => [(String, a)] -> DAWG a
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs

-- | Make DAWG from the list of words.  Annotate each word with
-- the @()@ value.
fromLang :: [String] -> DAWG ()
fromLang xs = fromList [(x, ()) | x <- xs]
