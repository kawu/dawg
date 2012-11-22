-- | A vector representation of 'M.Map'.

module Data.DAWG.VMap
( VMap (unVMap)
, empty
, lookup
, insert
, fromList
, toList
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Data.Binary (Binary, put, get)
import Data.Vector.Binary ()
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U

-- | A strictly ascending vector of distinct elements with respect
-- to 'fst' values.
newtype VMap = VMap { unVMap :: U.Vector (Int, Int) }
    deriving (Show, Eq, Ord)

instance Binary VMap where
    put v = put (unVMap v)
    get = VMap <$> get

-- | Empty map.
empty :: VMap
empty = VMap U.empty
{-# INLINE empty #-}

-- | Lookup the character in the map.
lookup :: Int -> VMap -> Maybe Int
lookup x = fmap snd . U.find ((==x) . fst) . unVMap
{-# INLINE lookup #-}

-- | Insert the character/value pair into the map.
-- TODO: Optimize!  Use the invariant, that VMap is
-- kept in an ascending vector.
insert :: Int -> Int -> VMap -> VMap
insert x y
    = VMap . U.fromList . M.toAscList
    . M.insert x y
    . M.fromList . U.toList . unVMap
{-# INLINE insert #-}

-- | Smart 'VMap' constructor which ensures that the underlying vector is
-- strictly ascending with respect to 'fst' values.
fromList :: [(Int, Int)] -> VMap
fromList = VMap . U.fromList . M.toAscList  . M.fromList 
{-# INLINE fromList #-}

-- | Convert the 'VMap' to a list of ascending character/value pairs.
toList :: VMap -> [(Int, Int)]
toList = U.toList . unVMap
{-# INLINE toList #-}
