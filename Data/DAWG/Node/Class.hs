{-# LANGUAGE MultiParamTypeClasses #-}

-- | Class abstracting automata nodes.

module Data.DAWG.Node.Class
( Node (..)
, children
) where

import Data.Binary (Binary)
import Data.DAWG.Types

class (Ord a, Ord (n a), Binary (n a)) => Node n a where
    -- | Value in leaf node.
    value   :: n a -> Maybe a
    -- | Epsilon transition.
    eps     :: n a -> ID
    -- | Transition on alphabet symbol.
    onSym   :: Sym -> n a -> Maybe ID
    -- | List of edges (symbol/ID pairs) outgoing from the node.
    edges   :: n a -> [(Sym, ID)]

    -- | Construct a leaf node.
    leaf    :: Maybe a -> n a
    -- | Make an empty branch with a given epsilon transition.
    branch  :: ID -> n a
    -- | Overwrite eps transition.
    withEps :: ID -> n a -> n a
    -- | Substitue edge determined by a given symbol.
    subst   :: Sym -> ID -> n a -> n a

-- | Children identifiers.
children :: Node n a => n a -> [ID]
children = map snd . edges
{-# INLINE children #-}
