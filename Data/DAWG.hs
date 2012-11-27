-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.
--
-- Transition backend has to be specified by a type signature.  You can import
-- the desired transition type and define your own dictionary construction
-- function.
--
-- > import Data.DAWG
-- > import Data.DAWG.Trans.Map (Trans)
-- >
-- > mkDict :: (Enum a, Ord b) => [([a], b)] -> DAWG Trans a b
-- > mkDict = fromList

module Data.DAWG
(
-- * DAWG type
  DAWG
, MkNode
-- * Query
, numStates
, lookup
-- * Construction
, empty
, fromList
, fromListWith
, fromLang
-- ** Insertion
, insert
, insertWith
-- ** Deletion
, delete
-- * Conversion
, assocs
, keys
, elems
) where

import Prelude hiding (lookup)
import Data.DAWG.Internal
