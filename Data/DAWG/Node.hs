{-# LANGUAGE RecordWildCards #-}

-- | Internal representation of automata nodes.

module Data.DAWG.Node
(
-- * Basic types
  ID
, Sym
-- * Node
, Node (..)
, onSym
, trans
, edges
, subst
, toGeneric
-- * Edge
, Edge
, to
, label
, annotate
, labeled
) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Data.Binary (Binary, Get, put, get)
import Data.Vector.Unboxed (Unbox)

import qualified Data.DAWG.VMap as M
import qualified Data.DAWG.Node.Specialized as N

-- | Node identifier.
type ID = Int

-- | Internal representation of the transition symbol.
type Sym = Int

-- | Edge with label.
type Edge a = (ID, a)

to :: Edge a -> ID
to = fst
{-# INLINE to #-}

label :: Edge a -> a
label = snd
{-# INLINE label #-}

annotate :: a -> Edge b -> Edge a
annotate x (i, _) = (i, x)
{-# INLINE annotate #-}

labeled :: a -> ID -> Edge a
labeled x i = (i, x)
{-# INLINE labeled #-}

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
data Node a b 
    = Branch {
        -- | Epsilon transition.
          eps       :: {-# UNPACK #-} !ID
        -- | Labeled edges outgoing from the node.
        , edgeMap   :: !(M.VMap b) }
    | Leaf { value  :: !a }
    deriving (Show, Eq, Ord)

instance (Unbox b, Binary a, Binary b) => Binary (Node a b) where
    put Branch{..} = put (1 :: Int) >> put eps >> put edgeMap
    put Leaf{..}   = put (2 :: Int) >> put value
    get = do
        x <- get :: Get Int
        case x of
            1 -> Branch <$> get <*> get
            _ -> Leaf <$> get

-- | Transition function.
onSym :: Unbox b => Sym -> Node a b -> Maybe b
onSym x (Branch _ es)   = M.lookup x es
onSym _ (Leaf _)        = Nothing
{-# INLINE onSym #-}

-- List of symbol/edge pairs outgoing from the node.
trans :: Unbox b => Node a b -> [(Sym, b)]
trans (Branch _ es)     = M.toList es
trans (Leaf _)          = []
{-# INLINE trans #-}

-- | List of outgoing edges.
edges :: Unbox b => Node a b -> [b]
edges = map snd . trans
{-# INLINE edges #-}

-- | Substitue edge determined by a given symbol.
subst :: Unbox b => Sym -> b -> Node a b -> Node a b
subst x e (Branch w es) = Branch w (M.insert x e es)
subst _ _ l             = l
{-# INLINE subst #-}

-- Yield generic version of a specialized node.
toGeneric :: N.Node a -> Node a (Edge ())
toGeneric N.Leaf{..}    = Leaf value
toGeneric N.Branch{..}  = Branch eps (annEdges edgeMap) where
    annEdges = M.fromList . map annEdge . M.toList
    annEdge = second (labeled ())
