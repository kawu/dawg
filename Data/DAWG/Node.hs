{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Internal representation of automata nodes.

module Data.DAWG.Node
( Node (..)
, onSym
, onSym'
, edges
, children
, insert
, reID
) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Data.Binary (Binary, Get, put, get)
import Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U

import Data.DAWG.Types
import Data.DAWG.Util (combine)
import Data.DAWG.Trans (Trans)
import Data.DAWG.HashMap (Hash, hash)
import qualified Data.DAWG.Trans.Hashed as H
import qualified Data.DAWG.Trans as T
import qualified Data.DAWG.Trans.Vector as TV
import qualified Data.DAWG.Trans.Map as TM

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
data Node t a b
    = Branch {
        -- | Epsilon transition.
          eps       :: {-# UNPACK #-} !ID
        -- | Transition map (outgoing edges).
        , transMap  :: !(H.Hashed t)
        -- | Labels corresponding to individual edges.
        , labelVect :: !(U.Vector a) }
    | Leaf { value  :: !(Maybe b) }
    deriving (Show)

deriving instance (Eq a, Eq b, U.Unbox a)   => Eq (Node TV.Trans a b)
deriving instance (Ord a, Ord b, U.Unbox a) => Ord (Node TV.Trans a b)
deriving instance (Eq a, Eq b, U.Unbox a)   => Eq (Node TM.Trans a b)
deriving instance (Ord a, Ord b, U.Unbox a) => Ord (Node TM.Trans a b)

instance (Trans t, Ord (Node t a b)) => Hash (Node t a b) where
    hash Branch{..} = combine eps (H.hash transMap)
    hash Leaf{..}   = case value of
    	Just _	-> (-1)
	Nothing	-> (-2)

instance (U.Unbox a, Binary t, Binary a, Binary b) => Binary (Node t a b) where
    put Branch{..} = put (1 :: Int) >> put eps >> put transMap >> put labelVect
    put Leaf{..}   = put (2 :: Int) >> put value
    get = do
        x <- get :: Get Int
        case x of
            1 -> Branch <$> get <*> get <*> get
            _ -> Leaf <$> get

-- | Transition function.
onSym :: Trans t => Sym -> Node t a b -> Maybe ID
onSym x (Branch _ t _)  = T.lookup x t
onSym _ (Leaf _)        = Nothing
{-# INLINE onSym #-}

-- | Transition function.
onSym' :: (Trans t, U.Unbox a) => Sym -> Node t a b -> Maybe (ID, a)
onSym' x (Branch _ t ls)   = do
    k <- T.index x t
    (,) <$> (snd <$> T.byIndex k t)
        <*> ls U.!? k
onSym' _ (Leaf _)           = Nothing
{-# INLINE onSym' #-}

-- | List of symbol/edge pairs outgoing from the node.
edges :: Trans t => Node t a b -> [(Sym, ID)]
edges (Branch _ t _)    = T.toList t
edges (Leaf _)          = []
{-# INLINE edges #-}

-- | List of children identifiers.
children :: Trans t => Node t a b -> [ID]
children = map snd . edges
{-# INLINE children #-}

-- | Substitue edge determined by a given symbol.
insert :: Trans t => Sym -> ID -> Node t a b -> Node t a b
insert x i (Branch w t ls)  = Branch w (T.insert x i t) ls
insert _ _ l                = l
{-# INLINE insert #-}

-- | Assign new identifiers.
reID :: Trans t => (ID -> ID) -> Node t a b -> Node t a b
reID _ (Leaf x)         = Leaf x
reID f (Branch e t ls)  =
    let reTrans = T.fromList . map (second f) . T.toList
    in  Branch (f e) (reTrans t) ls
