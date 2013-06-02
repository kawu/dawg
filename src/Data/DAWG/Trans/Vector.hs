{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A vector representation of a transition map.  Memory efficient, but the
-- insert operation is /O(n)/ with respect to the number of transitions.
-- In particular, complexity of the insert operation can make the construction
-- of a large-alphabet dictionary intractable.

module Data.DAWG.Trans.Vector
( Trans (unTrans)
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Data.Binary (Binary)
import Data.Vector.Binary ()
import qualified Data.IntMap as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.DAWG.Types
import Data.DAWG.Util
import qualified Data.DAWG.Trans as C

-- | A vector of distinct key/value pairs strictly ascending with respect
-- to key values.
newtype Trans = Trans { unTrans :: U.Vector (Sym, ID) }
    deriving (Show, Eq, Ord, Binary)

instance C.Trans Trans where
    empty = Trans U.empty
    {-# INLINE empty #-}

    lookup x m = do
        k <- C.index x m
        snd <$> C.byIndex k m
    {-# INLINE lookup #-}

    index x (Trans v)
        = either Just (const Nothing) $
            binarySearch (flip compare x . fst) v
    {-# INLINE index #-}

    byIndex k (Trans v) = v U.!? k
    {-# INLINE byIndex #-}

    insert x y (Trans v) = Trans $
        case binarySearch (flip compare x . fst) v of
            Left k  -> U.modify (\w -> UM.write w k (x, y)) v
            Right k ->
                let (v'L, v'R) = U.splitAt k v
                in  U.concat [v'L, U.singleton (x, y), v'R]
    {-# INLINE insert #-}

    fromList = Trans . U.fromList . M.toAscList . M.fromList
    {-# INLINE fromList #-}

    toList = U.toList . unTrans
    {-# INLINE toList #-}
