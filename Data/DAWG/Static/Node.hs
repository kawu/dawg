{-# LANGUAGE RecordWildCards #-}

-- | Internal representation of static automata nodes.

module Data.DAWG.Static.Node
( Node(..)
, onSym
, onSym'
, edges
, children
, insert
, fromDyn
) where

import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, Get, put, get)
import Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U

import Data.DAWG.Types
import Data.DAWG.Trans.Vector (Trans)
import qualified Data.DAWG.Trans as T
import qualified Data.DAWG.Dynamic.Node as D

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
        -- | Transition map (outgoing edges).
        , transMap  :: !Trans
        -- | Labels corresponding to individual edges.
        , labelVect :: !(U.Vector a) }
    | Leaf { value  :: !(Maybe b) }
    deriving (Show, Eq, Ord)

instance (U.Unbox a, Binary a, Binary b) => Binary (Node a b) where
    put Branch{..} = put (1 :: Int) >> put eps >> put transMap >> put labelVect
    put Leaf{..}   = put (2 :: Int) >> put value
    get = do
        x <- get :: Get Int
        case x of
            1 -> Branch <$> get <*> get <*> get
            _ -> Leaf <$> get

-- | Transition function.
onSym :: Sym -> Node a b -> Maybe ID
onSym x (Branch _ t _)  = T.lookup x t
onSym _ (Leaf _)        = Nothing
{-# INLINE onSym #-}

-- | Transition function.
onSym' :: U.Unbox a => Sym -> Node a b -> Maybe (ID, a)
onSym' x (Branch _ t ls)   = do
    k <- T.index x t
    (,) <$> (snd <$> T.byIndex k t)
        <*> ls U.!? k
onSym' _ (Leaf _)           = Nothing
{-# INLINE onSym' #-}

-- | List of symbol/edge pairs outgoing from the node.
edges :: Node a b -> [(Sym, ID)]
edges (Branch _ t _)    = T.toList t
edges (Leaf _)          = []
{-# INLINE edges #-}

-- | List of children identifiers.
children :: Node a b -> [ID]
children = map snd . edges
{-# INLINE children #-}

-- | Substitue edge determined by a given symbol.
insert :: Sym -> ID -> Node a b -> Node a b
insert x i (Branch w t ls)  = Branch w (T.insert x i t) ls
insert _ _ l                = l
{-# INLINE insert #-}

-- | Make "static" node from a "dynamic" node.
fromDyn
    :: (ID -> ID)   -- ^ Assign new IDs 
    -> D.Node b     -- ^ "Dynamic" node
    -> Node () b    -- ^ "Static" node
fromDyn _ (D.Leaf x)        = Leaf x
fromDyn f (D.Branch e t)    =
    let reTrans = T.fromList . map (second f) . T.toList
    in  Branch (f e) (reTrans t) U.empty
