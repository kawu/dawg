-- | The module provides an abstraction over transition maps from
-- alphabet symbols to node identifiers.

module Data.DAWG.Trans
( Trans (..)
) where

import Data.DAWG.Types

-- | Abstraction over transition maps from alphabet symbols to
-- node identifiers.
class Trans t where
    -- | Empty transition map.
    empty       :: t
    -- | Lookup sybol in the map.
    lookup      :: Sym -> t -> Maybe ID
    -- | Find index of the symbol.
    index       :: Sym -> t -> Maybe Int
    -- | Select a (symbol, ID) pair by index of its position in the map.
    byIndex     :: Int -> t -> Maybe (Sym, ID)
    -- | Insert element to the transition map.
    insert      :: Sym -> ID -> t -> t
    -- | Construct transition map from a list.
    fromList    :: [(Sym, ID)] -> t
    -- | Translate transition map into a list.
    toList      :: t -> [(Sym, ID)]
