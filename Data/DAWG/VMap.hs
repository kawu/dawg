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
newtype VMap a = VMap { unVMap :: U.Vector (Char, a) }
    deriving (Show, Eq, Ord)

instance (Binary a, U.Unbox a) => Binary (VMap a) where
    put v = put (unVMap v)
    get = VMap <$> get

-- | Empty map.
empty :: U.Unbox a => VMap a
empty = VMap U.empty
{-# INLINE empty #-}

-- | Lookup the character in the map.
lookup :: U.Unbox a => Char -> VMap a -> Maybe a
lookup x = fmap snd . U.find ((==x) . fst) . unVMap
{-# INLINE lookup #-}

-- | Insert the character/value pair into the map.
-- TODO: Optimize!  Use the invariant, that VMap is
-- kept in an ascending vector.
insert :: U.Unbox a => Char -> a -> VMap a -> VMap a
insert x y
    = VMap . U.fromList . M.toAscList
    . M.insert x y
    . M.fromList . U.toList . unVMap
{-# INLINE insert #-}

-- | Smart 'VMap' constructor which ensures that the underlying vector is
-- strictly ascending with respect to 'fst' values.
fromList :: U.Unbox a => [(Char, a)] -> VMap a
fromList = VMap . U.fromList . M.toAscList  . M.fromList 
{-# INLINE fromList #-}

-- | Convert the 'VMap' to a list of ascending character/value pairs.
toList :: U.Unbox a => VMap a -> [(Char, a)]
toList = U.toList . unVMap
{-# INLINE toList #-}
