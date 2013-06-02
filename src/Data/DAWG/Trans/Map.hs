{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Implementation of a transition map build on top of the "M.Map" container.

module Data.DAWG.Trans.Map
( Trans (unTrans)
) where

import Prelude hiding (lookup)
import Data.Binary (Binary)
import qualified Data.Map as M

import Data.DAWG.Types
import qualified Data.DAWG.Trans as C

-- | A vector of distinct key/value pairs strictly ascending with respect
-- to key values.
newtype Trans = Trans { unTrans :: M.Map Sym ID }
    deriving (Show, Eq, Ord, Binary)

instance C.Trans Trans where
    empty = Trans M.empty
    {-# INLINE empty #-}

    lookup x = M.lookup x . unTrans
    {-# INLINE lookup #-}

    index x = M.lookupIndex x . unTrans
    {-# INLINE index #-}

    byIndex i (Trans m) =
	let n = M.size m
        in  if i >= 0 && i < n
                then Just (M.elemAt i m)
                else Nothing
    {-# INLINE byIndex #-}

    insert x y (Trans m) = Trans (M.insert x y m)
    {-# INLINE insert #-}

    fromList = Trans . M.fromList
    {-# INLINE fromList #-}

    toList = M.toList . unTrans
    {-# INLINE toList #-}
