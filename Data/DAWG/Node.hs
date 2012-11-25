{-# LANGUAGE RecordWildCards #-}

-- | Internal representation of automata nodes.

module Data.DAWG.Node
( Sym
, Node (..)
, ID
, onSym
, onSym'
, trans
, edges
, subst
, reIdent
) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Data.Binary (Binary, Get, put, get)
import qualified Data.Vector.Unboxed as U

import Data.DAWG.Types
import qualified Data.DAWG.VMap as M

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
        -- | Edges outgoing from the node.
        , edgeMap   :: !(M.VMap ID)
        -- | Labels corresponding to individual edges.
        , labelVect :: !(U.Vector b) }
    | Leaf { value  :: !a }
    deriving (Show, Eq, Ord)

instance (U.Unbox b, Binary a, Binary b) => Binary (Node a b) where
    put Branch{..} = put (1 :: Int) >> put eps >> put edgeMap >> put labelVect
    put Leaf{..}   = put (2 :: Int) >> put value
    get = do
        x <- get :: Get Int
        case x of
            1 -> Branch <$> get <*> get <*> get
            _ -> Leaf <$> get

-- | Transition function.
onSym :: Sym -> Node a b -> Maybe ID
onSym x (Branch _ es _) = M.lookup x es
onSym _ (Leaf _)        = Nothing
{-# INLINE onSym #-}

-- | Transition function.
onSym' :: U.Unbox b => Sym -> Node a b -> Maybe (ID, b)
onSym' x (Branch _ es ls)   = do
    k <- M.index x es
    (,) <$> (snd <$> M.byIndex k es)
        <*> ls U.!? k
onSym' _ (Leaf _)           = Nothing
{-# INLINE onSym' #-}

-- | List of symbol/edge pairs outgoing from the node.
trans :: Node a b -> [(Sym, ID)]
trans (Branch _ es _)   = M.toList es
trans (Leaf _)          = []
{-# INLINE trans #-}

-- | List of outgoing edges.
edges :: Node a b -> [ID]
edges = map snd . trans
{-# INLINE edges #-}

-- | Substitue edge determined by a given symbol.
subst :: Sym -> ID -> Node a b -> Node a b
subst x i (Branch w es ts)  = Branch w (M.insert x i es) ts
subst _ _ l                 = l
{-# INLINE subst #-}

-- | Assign new identifiers.
reIdent :: (ID -> ID) -> Node a b -> Node a b
reIdent _ (Leaf x)          = Leaf x
reIdent f (Branch e es ts)  =
    let reEdges = M.fromList . map (second f) . M.toList
    in  Branch (f e) (reEdges es) ts
