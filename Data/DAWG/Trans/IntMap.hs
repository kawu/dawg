{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A vector representation of a transition map.

module Data.DAWG.Trans.IntMap
( Trans (unTrans)
) where

import Prelude hiding (lookup)
import Data.Binary (Binary)
import qualified Data.IntMap as M

import Data.DAWG.Types
import qualified Data.DAWG.Trans as C

-- | A vector of distinct key/value pairs strictly ascending with respect
-- to key values.
newtype Trans = Trans { unTrans :: M.IntMap ID }
    deriving (Show, Eq, Ord, Binary)

instance C.Trans Trans where
    empty = Trans M.empty
    {-# INLINE empty #-}

    lookup x (Trans m) = M.lookup x m
    {-# INLINE lookup #-}

    insert x y (Trans m) = Trans $ M.insert x y m
    {-# INLINE insert #-}

    fromList = Trans . M.fromList
    {-# INLINE fromList #-}

    toList = M.toList . unTrans
    {-# INLINE toList #-}

    index   _ _ = error "Trans.IntMap doesn't provide the 'index' operation"
    byIndex _ _ = error "Trans.IntMap doesn't provide the 'byIndex' operation"
