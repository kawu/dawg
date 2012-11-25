-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.

module Data.DAWG
(
-- * DAWG type
  DAWG
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
