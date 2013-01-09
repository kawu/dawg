-- | The module exports internal representation of dynamic DAWG.

module Data.DAWG.Dynamic.Internal
(
-- * DAWG type
  DAWG (..)
) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, put, get)

import Data.DAWG.Types
import Data.DAWG.Graph (Graph)
import qualified Data.DAWG.Dynamic.Node as N

-- | A directed acyclic word graph with phantom type @a@ representing
-- type of alphabet elements.
data DAWG a b = DAWG
    { graph :: !(Graph (N.Node b))
    , root  :: !ID }
    deriving (Show, Eq, Ord)

instance (Ord b, Binary b) => Binary (DAWG a b) where
    put d = do
        put (graph d)
        put (root d)
    get = DAWG <$> get <*> get
