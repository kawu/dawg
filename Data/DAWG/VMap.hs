{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

-- | A vector representation of 'M.Map'.

module Data.DAWG.VMap
( VMap (unVMap)
, empty
, lookup
, index
, byIndex
, insert
, findLastLE
, fromList
, toList
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Data.Bits (shiftR)
import Data.Binary (Binary, put, get)
import Data.Vector.Binary ()
import Data.Vector.Unboxed (Unbox)
import qualified Control.Monad.ST as ST
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | A strictly ascending vector of distinct elements with respect
-- to 'fst' values.
newtype VMap a = VMap { unVMap :: U.Vector (Int, a) }
    deriving (Show, Eq, Ord)

instance (Binary a, Unbox a) => Binary (VMap a) where
    put v = put (unVMap v)
    get = VMap <$> get

-- | Empty map.
empty :: Unbox a => VMap a
empty = VMap U.empty
{-# INLINE empty #-}

-- | Lookup the symbol in the map.
lookup :: Unbox a => Int -> VMap a -> Maybe a
lookup x m = do
    k <- index x m
    snd <$> byIndex k m
{-# INLINE lookup #-}

-- | Lookup the symbol in the map.
index :: Unbox a => Int -> VMap a -> Maybe Int
index x (VMap v)
    = either Just (const Nothing) $
        binarySearch (flip compare x . fst) v
{-# INLINE index #-}

-- | Lookup key/value pair by vector index.
byIndex :: Unbox a => Int -> VMap a -> Maybe (Int, a)
byIndex k (VMap v) = v U.!? k
{-# INLINE byIndex #-}

-- | Insert the symbol/value pair into the map.
insert :: Unbox a => Int -> a -> VMap a -> VMap a
insert x y (VMap v) = VMap $
    case binarySearch (flip compare x . fst) v of
        Left k  -> U.modify (\w -> UM.write w k (x, y)) v
        Right k ->
            let (v'L, v'R) = U.splitAt k v
            in  U.concat [v'L, U.singleton (x, y), v'R]
{-# INLINE insert #-}

-- | Given a vector sorted with respect to some underlying comparison
-- function, find last element which is not 'GT' with respect to the
-- comparison function.
findLastLE :: Unbox a => (a -> Ordering) -> U.Vector a -> Maybe (Int, a)
findLastLE cmp v =
    let k' = binarySearch cmp v
    	k  = either id (\x -> x-1) k'
    in  (k,) <$> v U.!? k
-- findLastLE :: Unbox a => (a -> Ordering) -> VMap a -> Maybe (Int, a)
-- findLastLE cmp (VMap v) =
--     let k = binarySearch (cmp . snd) v
--     in  v U.!? either id (\x -> x-1) k
{-# INLINE findLastLE #-}

-- | Given a vector of length @n@ strictly ascending with respect to a given
-- comparison function, find an index at which the given element could be
-- inserted while preserving sortedness.
-- The 'Left' result indicates, that the 'EQ' element has been found,
-- while the 'Right' result means otherwise.  Value of the 'Right'
-- result is in the [0,n] range.
binarySearch :: Unbox a => (a -> Ordering) -> U.Vector a -> Either Int Int
binarySearch cmp v = ST.runST $ do
    w <- U.unsafeThaw v
    search w
  where
    search w =
        loop 0 (UM.length w)
      where
        loop !l !u
            | u <= l    = return (Right l)
            | otherwise = do
                let k = (u + l) `shiftR` 1
                x <- UM.unsafeRead w k
                case cmp x of
                    LT -> loop (k+1) u
                    EQ -> return (Left k)
                    GT -> loop l     k
{-# INLINE binarySearch #-}

-- | Smart 'VMap' constructor which ensures that the underlying vector is
-- strictly ascending with respect to 'fst' values.
fromList :: Unbox a => [(Int, a)] -> VMap a
fromList = VMap . U.fromList . M.toAscList . M.fromList
{-# INLINE fromList #-}

-- | Convert the 'VMap' to a list of ascending symbol/value pairs.
toList :: Unbox a => VMap a -> [(Int, a)]
toList = U.toList . unVMap
{-# INLINE toList #-}
