-- | A vector representation of 'M.Map'.

module Data.DAWG.VMap
( VMap (unVMap)
, mkVMap
, empty
, lookup
, insert
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

-- | Smart VMap constructor which ensures that the underlying vector is
-- strictly ascending with respect to 'fst' values.
mkVMap :: U.Unbox a => [(Char, a)] -> VMap a
mkVMap = VMap . U.fromList . M.toAscList  . M.fromList 
{-# INLINE mkVMap #-}

-- | Empty map.
empty :: U.Unbox a => VMap a
empty = VMap U.empty
{-# INLINE empty #-}

-- | Lookup the character in the map.
lookup :: U.Unbox a => Char -> VMap a -> Maybe a
lookup x = fmap snd . U.find ((==x) . fst) . unVMap
{-# INLINE lookup #-}

-- | Insert the (character, value) pair into the map.
-- TODO: Optimize!  Use the invariant, that VMap is
-- kept in an ascending vector.
insert :: U.Unbox a => Char -> a -> VMap a -> VMap a
insert x y
    = VMap . U.fromList . M.toAscList
    . M.insert x y
    . M.fromList . U.toList . unVMap
{-# INLINE insert #-}
