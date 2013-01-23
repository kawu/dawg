{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bucket of hash table entries with the same hash modulo table size.

module Data.DAWG.HashTable.Bucket
( Bucket
, new
, size
, assocs
, lookup
, lookupMember
, insert
, insertNew
, delete
, deleteMember
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$), (<$>), (<*>))
import Control.Monad.ST
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as U

import Data.DAWG.HashTable.Hash (Hash)
import qualified Data.DAWG.HashTable.Hash as H

-- | Bucket of entries with the same hash value module size of the hash table.
data Bucket s a b = Bucket {
      hashes    :: U.MVector s Int
    , keys      :: V.MVector s a
    , values    :: V.MVector s b }

-- | New bucket of given size.
new :: Int -> ST s (Bucket s a b)
new k = do
    hs <- U.new k
    U.set hs empty 
    Bucket hs <$> V.new k <*> V.new k

-- | Size of the bucket (it doesn't have to be equal to the
-- number of elements, though).
size :: Bucket s a b -> Int
size = U.length  . hashes
{-# INLINE size #-}

-- | Value representing empty bucket entry.
empty :: Int
empty = -1
{-# INLINE empty #-}

-- | Positive hash.
hashP :: Hash a => a -> Int
hashP = abs . H.hash
{-# INLINE hashP #-}

-- | Search index of bucket entry with the same key and its hash.
search :: Hash a => a -> Bucket s a b -> ST s (Maybe Int)
search k (Bucket hs ks _) = go 0
  where
    n = U.length hs
    h = hashP k
    go !i
        | i < n = do
            h' <- U.read hs i
            if h == h'
                then do
                    k' <- V.read ks i
                    if k == k'
                        then return (Just i)
                        else go (i+1)
                else go (i+1)
        | otherwise = return Nothing

-- | Search index of bucket entry with the same key and its hash.
-- It is assumed, that key is a member of the bucket.
searchMember :: Hash a => a -> Bucket s a b -> ST s (Maybe Int)
searchMember k (Bucket hs ks _) = consume =<< go [] 0
  where
    n = U.length hs
    h = hashP k
    go acc !i =
        if i < n
            then do
                h' <- U.read hs i
                if h == h'
                    then go (i:acc) (i+1)
                    else go    acc  (i+1)
            else return acc
    consume [i]     = return (Just i)
    consume (i:is)  = do
        k' <- V.read ks i
        if k == k'
            then return (Just i)
            else consume is
    consume []      = return Nothing
        
-- | List of key/elem pairs in the bucket.
assocs :: Bucket s a b -> ST s [(a, b)]
assocs Bucket{..} = go [] 0
  where
    n = U.length hashes
    go acc !i =
        if i < n
            then do
                k <- V.read keys i
                v <- V.read values i
                go ((k, v):acc) (i+1)
            else return acc

-- | Search empty place in the bucket. 
searchEmpty :: Bucket s b a -> ST s (Maybe Int)
searchEmpty (Bucket hs _ _) = go 0
  where
    n = U.length hs
    go !k
        | k < n = do
            x <- U.read hs k
            if x == empty
                then return (Just k)
                else go (k+1)
        | otherwise = return Nothing

-- | Double size of the bucket.
grow :: Bucket s a b -> ST s (Bucket s a b)
grow (Bucket hs ks vs) = do
    let n = U.length hs
    hs' <- U.grow hs n
    ks' <- V.grow ks n
    vs' <- V.grow vs n
    return $ Bucket hs' ks' vs'

-- | Insert key/value pair into the bucket.  Return new bucket if it has
-- been grown.
insert :: Hash a => a -> b -> Bucket s a b -> ST s (Bucket s a b)
insert k v b = do
    mix <- search k b
    case mix of
        Nothing -> insertNew k v b
        Just ix -> b <$ V.write (values b) ix v

-- | Insert new key/elem pair into the bucket.  It is assumed that the key
-- is new; otherwise, the function may work incorrectly.
insertNew :: Hash a => a -> b -> Bucket s a b -> ST s (Bucket s a b)
insertNew k v b = do
    mix <- searchEmpty b
    case mix of
        Nothing -> do
            let ix = size b
            b' <- grow b
            insertOn ix b'
            return b'
        Just ix -> do
            insertOn ix b
            return b
  where
    insertOn ix (Bucket hs ks vs) = do
        U.write hs ix (hashP k)
        V.write ks ix k
        V.write vs ix v

-- | Lookup key in the bucket.
lookup :: Hash a => a -> Bucket s a b -> ST s (Maybe b)
lookup k b = do
    mix <- search k b
    case mix of
        Nothing -> return Nothing
        Just ix -> Just <$> V.read (values b) ix

-- | Lookup key which is known to be a member of the bucket.
lookupMember :: Hash a => a -> Bucket s a b -> ST s (Maybe b)
lookupMember k b = do
    mix <- searchMember k b
    case mix of
        Nothing -> return Nothing
        Just ix -> Just <$> V.read (values b) ix

-- | Remove key from the bucket together with accompanying value.
delete :: Hash a => a -> Bucket s a b -> ST s (Bucket s a b)
delete k b = do
    mix <- search k b
    case mix of
        Nothing -> return b
        Just ix -> do
            let Bucket{..} = b
            U.write hashes ix empty
            V.write keys ix undefined
            V.write values ix undefined
            return b

-- | Remove key which is known to be a member of the bucket together
-- with accompanying value.
deleteMember :: Hash a => a -> Bucket s a b -> ST s (Bucket s a b)
deleteMember k b = do
    mix <- searchMember k b
    case mix of
        Nothing -> return b
        Just ix -> do
            let Bucket{..} = b
            U.write hashes ix empty
            V.write keys ix undefined
            V.write values ix undefined
            return b
