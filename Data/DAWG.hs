-- | The module provides implementation of /directed acyclic word graphs/
-- (DAWGs) also known as /minimal acyclic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.

module Data.DAWG
( DAWG (..)
, empty
, numStates
, insert
-- , insertWith
-- , delete
, lookup
, fromList
-- , fromListWith
, fromLang
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
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
insertM :: Ord a => String -> a -> Id -> GraphM a Id
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

-- insertWithM :: Ord a => (a -> a -> a) -> String -> a -> Id -> GraphM a Id
-- insertWithM f [] y i = do
--     n <- nodeBy i
--     deleteNode n
--     case G.value n of
--         Just y' ->
--             let y'' = f y y'
--             in  y'' `seq` insertNode (n { G.value = Just y'' })
--         Nothing -> insertNode (n { G.value = Just y })
-- insertWithM f (x:xs) y i = do
--     n <- nodeBy i
--     j <- case G.onChar x n of
--         Just j  -> return j
--         Nothing -> insertNode leaf
--     k <- insertWithM f xs y j
--     deleteNode n
--     insertNode (G.subst x k n)

-- deleteM :: Ord a => String -> Id -> GraphM a Id
-- deleteM [] i = do
--     n <- nodeBy i
--     deleteNode n
--     insertNode (n { G.value = Nothing })
-- deleteM (x:xs) i = do
--     n <- nodeBy i
--     case G.onChar x n of
--         Nothing -> return i
--         Just j  -> do
--             k <- deleteM xs j
--             deleteNode n
--             insertNode (G.subst x k n)
    
lookupM :: String -> Id -> GraphM a (Maybe a)
lookupM [] i = do
    j <- G.eps <$> nodeBy i
    G.unValue <$> nodeBy j
lookupM (x:xs) i = do
    n <- nodeBy i
    case G.onChar x n of
        Just j  -> lookupM xs j
        Nothing -> return Nothing

-- | A 'G.Graph' with one root from which all other graph nodes should
-- be accesible.
data DAWG a = DAWG
    { graph :: !(Graph (Maybe a))
    , root  :: !Id }
    deriving (Show, Eq, Ord)

-- instance (Ord a, Binary a) => Binary (DAWG a) where
--     put d = do
--         put (graph d)
--         put (root d)
--     get = DAWG <$> get <*> get

-- | Empty DAWG.
empty :: Ord a => DAWG a
empty = 
    let (i, g) = S.runState insertLeaf G.empty
    in  DAWG g i

-- | Number of states in the underlying graph.
numStates :: DAWG a -> Int
numStates = G.size . graph

-- | Insert the (key, value) pair into the DAWG.
insert :: Ord a => String -> a -> DAWG a -> DAWG a
insert xs y d =
    let (i, g) = S.runState (insertM xs y $ root d) (graph d)
    in  DAWG g i

-- -- | Insert with a function, combining new value and old value.
-- -- 'insertWith' f key value d will insert the pair (key, value) into d if
-- -- key does not exist in the DAWG. If the key does exist, the function
-- -- will insert the pair (key, f new_value old_value).
-- insertWith :: Ord a => (a -> a -> a) -> String -> a -> DAWG a -> DAWG a
-- insertWith f xs y d =
--     let (i, g) = S.runState (insertWithM f xs y $ root d) (graph d)
--     in  DAWG g i
-- 
-- -- | Delete the key from the DAWG.
-- delete :: Ord a => String -> DAWG a -> DAWG a
-- delete xs d =
--     let (i, g) = S.runState (deleteM xs $ root d) (graph d)
--     in  DAWG g i

-- | Find value associated with the key.
lookup :: String -> DAWG a -> Maybe a
lookup xs d = S.evalState (lookupM xs $ root d) (graph d)

-- | Construct DAWG from the list of (word, value) pairs.
fromList :: Ord a => [(String, a)] -> DAWG a
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs

-- -- | Construct DAWG from the list of (word, value) pairs
-- -- with a combining function.  The combining function is
-- -- applied strictly.
-- fromListWith :: Ord a => (a -> a -> a) -> [(String, a)] -> DAWG a
-- fromListWith f xs =
--     let update t (x, v) = insertWith f x v t
--     in  foldl' update empty xs

-- | Make DAWG from the list of words.  Annotate each word with
-- the @()@ value.
fromLang :: [String] -> DAWG ()
fromLang xs = fromList [(x, ()) | x <- xs]
