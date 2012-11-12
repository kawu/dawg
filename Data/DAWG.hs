-- | The module provides implementation of /directed acyclic word graphs/
-- (DAWGs) also known as /minimal acyclic finite-state automata/.
-- The implementation provides fast insert and (TODO:)delete operations
-- which can be used to build the DAWG structure incrementaly.

module Data.DAWG
( DAWG (..)
, empty
, size
, insert
, lookup
, fromList
, fromLang
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Data.List (foldl')
import qualified Control.Monad.State.Strict as S

import Data.DAWG.Graph (Id, Node, Graph)
import qualified Data.DAWG.Graph as G

type GraphM a b = S.State (Graph a) b

mkState :: (Graph a -> Graph a) -> Graph a -> ((), Graph a)
mkState f g = ((), f g)

-- | Return node with the given identifier.
nodeBy :: Id -> GraphM a (Node a)
nodeBy i = G.nodeBy i <$> S.get

-- Evaluate the 'G.insert' function within the monad.
insertNode :: Ord a => Node a -> GraphM a Id
insertNode = S.state . G.insert

-- Evaluate the 'G.delete' function within the monad.
deleteNode :: Ord a => Node a -> GraphM a ()
deleteNode = S.state . mkState . G.delete

insertM :: Ord a => String -> a -> Id -> GraphM a Id
insertM [] y i = do
    n <- nodeBy i
    deleteNode n
    insertNode (n { G.value = Just y })
insertM (x:xs) y i = do
    n <- nodeBy i
    j <- case G.onChar x n of
        Just j  -> return j
        Nothing -> insertNode G.leaf
    k <- insertM xs y j
    deleteNode n
    insertNode (G.subst x k n)
    
lookupM :: String -> Id -> GraphM a (Maybe a)
lookupM [] i = G.value <$> nodeBy i
lookupM (x:xs) i = do
    n <- nodeBy i
    case G.onChar x n of
        Just j  -> lookupM xs j
        Nothing -> return Nothing

-- | A 'G.Graph' with one root from which all other graph nodes should
-- be accesible.
data DAWG a = DAWG
    { graph :: !(Graph a)
    , root  :: !Id }
    deriving (Show, Eq, Ord)

-- | Empty DAWG.
empty :: DAWG a
empty = DAWG G.empty 0

-- | DAWG size (number of nodes).
size :: DAWG a -> Int
size = G.size . graph

-- | Insert the (key, value) pair into the DAWG.
insert :: Ord a => String -> a -> DAWG a -> DAWG a
insert xs y d =
    let (i, g) = S.runState (insertM xs y $ root d) (graph d)
    in  DAWG g i

-- | Find value associated with the key.
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
