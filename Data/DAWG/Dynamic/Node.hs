{-# LANGUAGE RecordWildCards #-}

-- | Internal representation of dynamic automata nodes.

module Data.DAWG.Dynamic.Node
( Node(..)
, onSym
, edges
, children
, insert
) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, Get, put, get)

import Data.DAWG.Types
import Data.DAWG.Util (combine)
import Data.DAWG.Trans.Map (Trans)
import Data.DAWG.HashTable.Hash 
import qualified Data.DAWG.Trans as T
import qualified Data.DAWG.Trans.Hashed as H

-- | Two nodes (states) belong to the same equivalence class (and,
-- consequently, they must be represented as one node in the graph)
-- iff they are equal with respect to their values and outgoing
-- edges.
--
-- Since 'Leaf' nodes are distinguished from 'Branch' nodes, two values
-- equal with respect to '==' function are always kept in one 'Leaf'
-- node in the graph.  It doesn't change the fact that to all 'Branch'
-- nodes one value is assigned through the epsilon transition.
--
-- Invariant: the 'eps' identifier always points to the 'Leaf' node.
-- Edges in the 'edgeMap', on the other hand, point to 'Branch' nodes.
data Node a
    = Branch {
        -- | Epsilon transition.
          eps       :: {-# UNPACK #-} !ID
        -- | Transition map (outgoing edges).
        , transMap  :: !(H.Hashed Trans) }
    | Leaf { value  :: !(Maybe a) }
    deriving (Show, Eq, Ord)

instance Ord a => Hash (Node a) where
    hash Branch{..} = combine eps (H.hash transMap)
    -- TODO: Will not work properly with the same hash for all
    -- Just values!
    hash Leaf{..}   = case value of
    	Just _	-> (-1)
	Nothing	-> (-2)

instance Binary a => Binary (Node a) where
    put Branch{..} = put (1 :: Int) >> put eps >> put transMap
    put Leaf{..}   = put (2 :: Int) >> put value
    get = do
        x <- get :: Get Int
        case x of
            1 -> Branch <$> get <*> get
            _ -> Leaf <$> get

-- | Transition function.
onSym :: Sym -> Node a -> Maybe ID
onSym x (Branch _ t)    = T.lookup x t
onSym _ (Leaf _)        = Nothing
{-# INLINE onSym #-}

-- | List of symbol/edge pairs outgoing from the node.
edges :: Node a -> [(Sym, ID)]
edges (Branch _ t)  = T.toList t
edges (Leaf _)      = []
{-# INLINE edges #-}

-- | List of children identifiers.
children :: Node a -> [ID]
children = map snd . edges
{-# INLINE children #-}

-- | Substitue edge determined by a given symbol.
insert :: Sym -> ID -> Node a -> Node a
insert x i (Branch w t) = Branch w (T.insert x i t)
insert _ _ l            = l
{-# INLINE insert #-}
