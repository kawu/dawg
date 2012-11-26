{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- | A vector representation of a transition map.

module Data.DAWG.Trans.Map
( Trans (unTrans)
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Data.List (foldl')
import Data.Monoid (Monoid, mappend)
import Data.Binary (Binary, put, get)
import qualified Data.Map as M

import Data.DAWG.Util
import Data.DAWG.Types
import qualified Data.DAWG.Trans as C

-- | A vector of distinct key/value pairs strictly ascending with respect
-- to key values.
--
-- Hash of a transition map is a sum of element-wise hashes.
-- Hash for a given element @(Sym, ID) = combine Sym ID@.
data Trans = Trans
    { hashSum   :: Int
    , unTrans   :: M.Map Sym ID }
    deriving (Show)

instance Binary Trans where
    put Trans{..} = put hashSum >> put unTrans
    get = Trans <$> get <*> get

instance Eq Trans where
    Trans i x == Trans j y
        =  i == j
        && M.size x == M.size y
        && M.toAscList x == M.toAscList y

instance Ord Trans where
    Trans _ x `compare` Trans _ y
        =  (M.size x `compare` M.size y)
        <> (M.toAscList x `compare` M.toAscList y)

(<>) :: Monoid m => m -> m -> m
x <> y = mappend x y
{-# INLINE (<>) #-}

instance C.Trans Trans where
    empty = Trans 0 M.empty
    {-# INLINE empty #-}

    hash = hashSum
    {-# INLINE hash #-}

    lookup x (Trans _ m)    = M.lookup x m
    {-# INLINE lookup #-}

    insert x y (Trans h m)  = Trans
        (h - h' + combine x y)
        (M.insert x y m)
      where
        h' = case M.lookup x m of
            Just y' -> combine x y'
            Nothing -> 0
    {-# INLINE insert #-}

    -- TODO: Reimplement.
    fromList =
        let f m (x, y) = C.insert x y m
        in  foldl' f C.empty
    -- fromList = Trans . M.fromList
    {-# INLINE fromList #-}

    toList = M.toList . unTrans
    {-# INLINE toList #-}

    index   _ _ = error "Trans.Map doesn't provide the 'index' operation"
    byIndex _ _ = error "Trans.Map doesn't provide the 'byIndex' operation"
