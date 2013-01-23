{-# LANGUAGE RecordWildCards #-}

-- | A growing hash table implementation.

module Data.DAWG.HashTable
( HashTable (..)
, new
, lookup
, lookupMember
, insertNew
, deleteMember
) where

import Prelude hiding (lookup)
import Control.Monad (forM_)
import Control.Monad.ST
import qualified Data.Vector.Mutable as V

import Data.DAWG.HashTable.Hash
import qualified Data.DAWG.HashTable.Bucket as B

import Debug.Trace (trace)

data HashTable s a b = HashTable {
    -- | Number of elements in the table.
      elemNum           :: !Int
    -- | Initial size of buckets.
    , bucketInitSize    :: !Int
    -- | A (maximal number of elements) to (table size) ratio.  After the
    -- effective ratio grows above this value, size of the table is doubled.
    , sizeRatio         :: !Double
    -- | The actual table.
    , body              :: V.MVector s (B.Bucket s a b) }

-- | New hash table with given sizeRatio and suggested size.
new
    :: Int      -- ^ Initial table size
    -> Int      -- ^ Initial bucket size
    -> Double   -- ^ Size ratio
    -> ST s (HashTable s a b)
new n m r = do
    body' <- V.new n
    forM_ [0..n-1] $ \i -> do
        bucket <- B.new m
        V.write body' i bucket
    return $ HashTable 0 m r body'

-- | Lookup key in the table.
lookup :: Hash a => a -> HashTable s a b -> ST s (Maybe b)
lookup k HashTable{..} = do
    let size = V.length body
        ix = hash k `mod` size
    bucket <- V.read body ix
    B.lookup k bucket

-- | Lookup key which is known to be a member of the table.
lookupMember :: Hash a => a -> HashTable s a b -> ST s b
lookupMember k HashTable{..} = do
    let ix = hash k `mod` V.length body
    bucket <- V.read body ix
    B.lookupMember k bucket

-- | Insert new element into the table.  It is assumed, that the key
-- is new; otherwise, the function may work incorrectly.
insertNew :: Hash a => a -> b -> HashTable s a b -> ST s (HashTable s a b)
insertNew k v tab0 = do
    tab@HashTable{..} <- checkGrow tab0
    let ix = hash k `mod` V.length body
    bucket  <- V.read body ix
    bucket' <- B.insertNew k v bucket
    V.write body ix bucket'
    return $ tab { elemNum = elemNum + 1 }

-- | Remove member key from the table.
deleteMember :: Hash a => a -> HashTable s a b -> ST s (HashTable s a b)
deleteMember k tab@HashTable{..} = do
    let ix = hash k `mod` V.length body
    bucket  <- V.read body ix
    bucket' <- B.deleteMember k bucket
    V.write body ix bucket'
    return $ tab { elemNum = elemNum - 1 }

-- | Grow the table if appropriate conditions are satisfied.
checkGrow :: Hash a => HashTable s a b -> ST s (HashTable s a b)
checkGrow tab@HashTable{..}
    | elemNum ./. V.length body > sizeRatio = grow tab
    | otherwise = return tab
  where
    x ./. y = fromIntegral x / fromIntegral y

-- | Grow the table.
-- TODO: does it work properly when the table is empty?
grow :: Hash a => HashTable s a b -> ST s (HashTable s a b)
grow tab0 = do
    let body0 = body tab0
        n0 = V.length body0
    tab1 <- trace ("grow table from size: " ++ show n0) $ new
        (n0 * 2)
        (bucketInitSize tab0)
        (sizeRatio tab0)
    -- TODO: This is tricky and should be changed; number of elements is
    -- incremented in the insertNew function, but we ignore it.
    -- Only when we exit from the grow function, the correct elenNum value
    -- is assigned.
    forM_ [0 .. n0-1] $ \i -> do
        bucket <- V.read body0 i
        assocs <- B.assocs bucket
        forM_ assocs $ \(k, v) -> do
            insertNew k v tab1
    trace "done" $ return $ tab1 { elemNum = elemNum tab0 }
