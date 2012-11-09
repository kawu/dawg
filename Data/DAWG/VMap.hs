-- | A vector representation of "Data.Map.Map". 

module Data.DAWG.VMap
( VMap (unVMap)
, mkVMap
, empty
, lookup
, insert
) where

import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U

-- | An strictly ascending vector of distinct elements with respect
-- to 'fst' values.
newtype VMap a = VMap { unVMap :: U.Vector (Char, a) }
    deriving (Show, Eq, Ord)

-- | Smart VMap constructor which ensures that the underlying vector is
-- strictly ascending with respect to 'fst' values.
mkVMap :: U.Unbox a => [(Char, a)] -> VMap a
mkVMap = VMap . U.fromList . M.toAscList  . M.fromList 
{-# INLINE mkVMap #-}

empty :: U.Unbox a => VMap a
empty = VMap U.empty
{-# INLINE empty #-}

lookup :: U.Unbox a => Char -> VMap a -> Maybe a
lookup x = fmap snd . U.find ((==x) . fst) . unVMap
{-# INLINE lookup #-}

-- | TODO: Optimize!  Use the invariant, that VMap is an ascending vector.
insert :: U.Unbox a => Char -> a -> VMap a -> VMap a
insert x y
    = VMap . U.fromList . M.toAscList
    . M.insert x y
    . M.fromList . U.toList . unVMap
{-# INLINE insert #-}
