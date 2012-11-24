{-# LANGUAGE RecordWildCards #-}

-- | Internal representation of automata nodes.

module Data.DAWG.Node2
(
-- * Basic types
  ID
, Sym
-- * Node
, Node (..)
, onSym
, trans
, edges
-- , children
, subst
-- , reIdent
) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, Get, put, get)
import Data.Vector.Unboxed (Unbox)

import qualified Data.DAWG.VMap as M

-- | Node identifier.
type ID = Int

-- | Internal representation of the transition symbol.
type Sym = Int

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
        -- | Labeled edges outgoing from the node.
        , edgeMap   :: !(M.VMap ID) }
    | Leaf { value  :: !a }
    deriving (Show, Eq, Ord)

instance (Binary a) => Binary (Node a) where
    put Branch{..} = put (1 :: Int) >> put eps >> put edgeMap
    put Leaf{..}   = put (2 :: Int) >> put value
    get = do
        x <- get :: Get Int
        case x of
            1 -> Branch <$> get <*> get
            _ -> Leaf <$> get

-- | Transition function.
onSym :: Sym -> Node a -> Maybe ID
onSym x (Branch _ es)   = M.lookup x es
onSym _ (Leaf _)        = Nothing
{-# INLINE onSym #-}

-- List of symbol/edge pairs outgoing from the node.
trans :: Node a -> [(Sym, ID)]
trans (Branch _ es)     = M.toList es
trans (Leaf _)          = []
{-# INLINE trans #-}

-- | List of outgoing edges.
edges :: Node a -> [ID]
edges = map snd . trans
{-# INLINE edges #-}

-- -- | List children identifiers.
-- children :: Unbox b => Node a b -> [ID]
-- children = map to . edges
-- {-# INLINE children #-}

-- | Substitue edge determined by a given symbol.
subst :: Sym -> ID -> Node a -> Node a
subst x e (Branch w es) = Branch w (M.insert x e es)
subst _ _ l             = l
{-# INLINE subst #-}

-- -- | Assign new identifiers.
-- reIdent :: Unbox b => (ID -> ID) -> Node a b -> Node a b
-- reIdent _ (Leaf x)      = Leaf x
-- reIdent f (Branch e es) =
--     let reEdges = M.fromList . map (second reEdge) . M.toList
--         reEdge (i, y) = (f i, y)
--     in  Branch (f e) (reEdges es)
