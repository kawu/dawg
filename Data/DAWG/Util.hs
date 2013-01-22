{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

-- | Utility functions.

module Data.DAWG.Util
( binarySearch
, findLastLE
-- , combine
) where

import Control.Applicative ((<$>))
import Data.Bits (shiftR, xor)
import Data.Vector.Unboxed (Unbox)
import qualified Control.Monad.ST as ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

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

-- | Given a vector sorted with respect to some underlying comparison
-- function, find last element which is not 'GT' with respect to the
-- comparison function.
findLastLE :: Unbox a => (a -> Ordering) -> U.Vector a -> Maybe (Int, a)
findLastLE cmp v =
    let k' = binarySearch cmp v
    	k  = either id (\x -> x-1) k'
    in  (k,) <$> v U.!? k
{-# INLINE findLastLE #-}

-- -- | Combine two given hash values.  'combine' has zero as a left
-- -- identity.
-- combine :: Int -> Int -> Int
-- combine h1 h2 = (h1 * 16777619) `xor` h2
-- {-# INLINE combine #-}
