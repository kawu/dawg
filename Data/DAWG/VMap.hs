{-# LANGUAGE BangPatterns #-}

-- | A vector representation of 'M.Map'.

module Data.DAWG.VMap
( VMap (unVMap)
, empty
, lookup
, findLastLE
, insert
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
lookup x (VMap v)
    | U.null v  = Nothing
    | otherwise = ST.runST $ do
        w <- U.unsafeThaw v
        fmap snd <$> search w x
  where
    search vec e =
        loop 0 (UM.length vec - 1)
      where
        loop !l !u
            | u <= l    = do
                e' <- UM.unsafeRead vec k
                return $ if e == fst e'
                    then (Just e')
                    else Nothing
            | otherwise = do
                e' <- UM.unsafeRead vec k
                case compare (fst e') e of
                    LT -> loop (k+1) u
                    EQ -> return (Just e')
                    GT -> loop l (k-1)
          where
            k = (u + l) `shiftR` 1
-- lookup x = fmap snd . U.find ((==x) . fst) . unVMap
{-# INLINE lookup #-}

-- | Find last map element which is not GT with respect to the
-- given ordering function.
findLastLE :: Unbox a => (a -> Ordering) -> VMap a -> Maybe (Int, a)
findLastLE cmp (VMap v) = ST.runST $ do
    w <- U.unsafeThaw v
    k <- search w
    return (v U.!? (k - 1))
  where
    search vec =
        loop 0 (UM.length vec)
      where
        loop !l !u
            | u <= l    = return l
            | otherwise = do
                let k = (u + l) `shiftR` 1
                x <- UM.unsafeRead vec k
                case cmp (snd x) of
                    LT -> loop (k+1) u
                    EQ -> return (k+1)
                    GT -> loop l     k
-- firstLL f x vm = do
--     k <-  U.findIndex ((>x) . f . snd) v
--       <|> if n > 0 then Just n else Nothing
--     return (v U.! (k - 1))
--   where
--     v = unVMap vm
--     n = U.length v
{-# INLINE findLastLE #-}

-- | Insert the symbol/value pair into the map.
-- TODO: Optimize! Use the invariant, that VMap is
-- kept in an ascending vector.
insert :: Unbox a => Int -> a -> VMap a -> VMap a
insert x y
    = VMap . U.fromList . M.toAscList
    . M.insert x y
    . M.fromList . U.toList . unVMap
{-# INLINE insert #-}

-- | Smart 'VMap' constructor which ensures that the underlying vector is
-- strictly ascending with respect to 'fst' values.
fromList :: Unbox a => [(Int, a)] -> VMap a
fromList = VMap . U.fromList . M.toAscList . M.fromList
{-# INLINE fromList #-}

-- | Convert the 'VMap' to a list of ascending symbol/value pairs.
toList :: Unbox a => VMap a -> [(Int, a)]
toList = U.toList . unVMap
{-# INLINE toList #-}
