-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
--
-- In comparison to "Data.DAWG" module the automaton implemented here:
--
--   * Keeps all nodes in one array and thus uses much less memory,
--
--   * Constitutes a /perfect hash automaton/ which provides
--     'hash' and 'unHash' functions,
--
--   * Doesn't provide insert/delete family of operations.
--
-- Use the 'freeze' and 'thaw' functions to convert between both DAWG
-- representations.

module Data.DAWG.Frozen
( 
) where

import qualified Data.DAWG.VMap as V

-- | State (node) of the automaton.
data Node a = Node
    { value :: !a
    , edges :: !V.VMap }
    deriving (Show, Eq, Ord)

-- | Root of stored on the first position of the array.
data DAWG a b = DAWG
    { 
