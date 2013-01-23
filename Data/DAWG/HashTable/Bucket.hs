{-# LANGUAGE BangPatterns #-}

-- | Bucket of hash table entries with the same hash modulo table size.

module Data.DAWG.HashTable.Bucket
( Bucket
, size
, insert
, insertNew
) where

import Control.Applicative ((<$))
import Control.Monad.ST
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as U

import Data.DAWG.HashTable.Hash (Hash)
import qualified Data.DAWG.HashTable.Hash as H

-- data Entry a b = Entry {
--     -- | Hash of the entry key.
--       keyHash   :: {-# UNPACK #-} !Int
--     -- | Entry key; not strict, since we want to be able to keep
--     -- undefined value here.
--     , key       :: a
--     -- | Entry value, possibly undefined.
--     , value     :: b }
--     deriving (Show, Eq, Ord)

-- | Bucket of entries with the same hash value module size of the hash table.
data Bucket s a b = Bucket {
      hashes    :: U.MVector s Int
    , keys      :: V.MVector s a
    , values    :: V.MVector s b }

size :: Bucket s a b -> Int
size = U.length  . hashes
{-# INLINE size #-}

-- | Positive hash.
hashP :: Hash a => a -> Int
hashP = abs . H.hash
{-# INLINE hashP #-}

-- | Search index of bucket entry with the same key and its hash.
searchKey :: a -> Bucket s a b -> ST s (Maybe Int)
searchKey = undefined

-- | Search empty place in the bucket. 
searchEmpty :: Bucket s b a -> ST s (Maybe Int)
searchEmpty (Bucket h _ _) = go 0
  where
    !n = U.length h
    go !k
        | k < n = do
            x <- U.read h k
            if x == (-1)
                then return (Just k)
                else go (k+1)
        | otherwise = return Nothing
    
-- | Double the size of the bucket.
grow :: Bucket s a b -> ST s (Bucket s a b)
grow = undefined

-- | Insert key/value pair into the bucket.  Return new bucket if it has
-- been grown.
insert :: Hash a => a -> b -> Bucket s a b -> ST s (Maybe (Bucket s a b))
insert k v b = do
    mix <- searchKey k b
    case mix of
        Nothing -> insertNew k v b
        Just ix -> Nothing <$ V.write (values b) ix v

-- | Insert new key/elem (that's an assumption) pair into the bucket.
insertNew :: Hash a => a -> b -> Bucket s a b -> ST s (Maybe (Bucket s a b))
insertNew k v b = do
    mix <- searchEmpty b
    case mix of
        Nothing -> do
            let ix = size b
            b' <- grow b
            insertOn ix b'
            return (Just b')
        Just ix -> do
            insertOn ix b
            return Nothing
  where
    insertOn ix (Bucket hs ks vs) = do
        U.write hs ix (hashP k)
        V.write ks ix k
        V.write vs ix v
