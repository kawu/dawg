{-# LANGUAGE RecordWildCards #-}

-- | A directed acyclic word graph.

module Data.DAWG.Unboxed
( DAWG (..)
, size
, nodeBy
, Node (..)
, entry
, charOn
, valueBy
, edges
, edgeOn
) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (listToMaybe)
import Data.Binary (Binary, get, put)
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Binary ()
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- | Index of a node.
type Ix = Int

-- | A directed acyclic word graph with character type 'Int' and dictionary
-- entry type @a@.  Each node is represented by a unique integer number
-- which is also an index of the node in the vector of DAWG nodes.
data DAWG a b = DAWG
    { root  :: Ix                   -- ^ Root (index) of the DAWG
    , nodes :: V.Vector (Node a b)  -- ^ Vector of DAWG nodes
    } deriving (Show)

-- | Size of the DAWG.
size :: DAWG a b -> Int
size = V.length . nodes
{-# INLINE size #-}

-- | Node by index.
nodeBy :: DAWG a b -> Ix -> Node a b
nodeBy dag k = nodes dag V.! k
{-# INLINE nodeBy #-}

-- | A node in the DAWG.
data Node a b = Node {
    -- | Value in the node.
    valueIn  :: b, 
    -- | Edges to subnodes (represented by DAWG node indices)
    -- annotated with characters.
    subNodes :: U.Vector (a, Ix)
    } deriving (Show)

-- | Value in the DAWG node represented by the index.
valueBy :: DAWG a b -> Ix -> b
valueBy dag k = valueIn (nodes dag V.! k)
{-# INLINE valueBy #-}

-- | Edges starting from the DAWG node represented by the index.
edges :: Unbox a => DAWG a b -> Ix -> [(a, Ix)]
edges dag k = U.toList . subNodes $ nodeBy dag k
{-# INLINE edges #-}

-- | Index of the node following the edge annotated with the
-- given character.
edgeOn :: (Eq a, Unbox a) => DAWG a b -> Int -> a -> Maybe Int
edgeOn DAWG{..} k x =
    let r = nodes V.! k
    in  snd <$> U.find ((x==).fst) (subNodes r)
{-# INLINE edgeOn #-}

-- | Return the dictionary entry determined by following the
-- path of node indices.
entry :: Unbox a => DAWG a (Maybe b) -> [Int] -> Maybe ([a], b)
entry dag xs = do
    x <- mapM (charOn dag) (zip (root dag:xs) xs)
    r <- maybeLast xs >>= valueBy dag 
    return (x, r)
  where
    maybeLast [] = Nothing
    maybeLast ys = Just $ last ys

-- | Determine the character on the edges between two nodes.
charOn :: Unbox a => DAWG a b -> (Int, Int) -> Maybe a
charOn dag (root, x) = listToMaybe
    [c | (c, y) <- edges dag root, x == y]

instance (Ord a, Unbox a, Binary a, Binary b) => Binary (Node a b) where
    put Node{..} = do
        put valueIn
        put subNodes
    get = Node <$> get <*> get

instance (Ord a, Unbox a, Binary a, Binary b) => Binary (DAWG a b) where
    put DAWG{..} = do
        put root
        put nodes
    get = DAWG <$> get <*> get
