-- | Hash type class definition.

module Data.DAWG.HashTable.Hash
( Hash (..)
) where

class Eq a => Hash a where
    hash    :: a -> Int
