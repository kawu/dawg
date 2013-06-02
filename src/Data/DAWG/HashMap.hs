{-# LANGUAGE RecordWildCards #-}

-- | A map from hashable keys to values.

module Data.DAWG.HashMap
( Hash (..)
, HashMap (..)
, empty
, lookup
, insertUnsafe
, lookupUnsafe
, deleteUnsafe
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, Get, put, get)
import qualified Data.Map as M
import qualified Data.IntMap as I

fromJust :: Maybe a -> a
fromJust (Just x)   = x
fromJust Nothing    = error "fromJust: Nothing"
{-# INLINE fromJust #-}

-- | Class for types which provide hash values.
class Ord a => Hash a where
    hash    :: a -> Int

-- | Value in a HashMap.
data Value a b
    = Single !a !b
    | Multi  !(M.Map a b)
    deriving (Show, Eq, Ord)

instance (Ord a, Binary a, Binary b) => Binary (Value a b) where
    put (Single x y)    = put (1 :: Int) >> put x >> put y
    put (Multi m)       = put (2 :: Int) >> put m
    get = do
        x <- get :: Get Int
        case x of
            1   -> Single <$> get <*> get
            _   -> Multi <$> get

-- | Find element associated to a value key.
find :: Ord a => a -> Value a b -> Maybe b
find x (Single x' y) = if x == x'
    then Just y
    else Nothing
find x (Multi m) = M.lookup x m

-- | Assumption: element is a member of the 'Value'. 
findUnsafe :: Ord a => a -> Value a b -> Maybe b
findUnsafe _ (Single _ y) = Just y	-- unsafe
findUnsafe x (Multi m) = M.lookup x m

-- | Convert map into a 'Single' form if possible.
trySingle :: Ord a => M.Map a b -> Value a b
trySingle m = if M.size m == 1
    then (uncurry Single) (M.findMin m)
    else Multi m

-- | Insert element into a value.
embed :: Ord a => a -> b -> Value a b -> Value a b
embed x y (Single x' y')    = Multi $ M.fromList [(x, y), (x', y')]
embed x y (Multi m)         = Multi $ M.insert x y m

-- | Delete element from a value.  Return 'Nothing' if the resultant
-- value is empty.
ejectUnsafe :: Ord a => a -> Value a b -> Maybe (Value a b)
ejectUnsafe _ (Single _ _)  = Nothing    -- unsafe
ejectUnsafe x (Multi m)     = (Just . trySingle) (M.delete x m)

-- | A map from /a/ keys to /b/ elements where keys instantiate the
-- 'Hash' type class.  Key/element pairs are kept in 'Value' objects
-- which takes care of potential hash collisions.
data HashMap a b = HashMap
    { size      :: {-# UNPACK #-} !Int
    , hashMap   :: !(I.IntMap (Value a b)) }
    deriving (Show, Eq, Ord)

instance (Ord a, Binary a, Binary b) => Binary (HashMap a b) where
    put HashMap{..} = put size >> put hashMap
    get = HashMap <$> get <*> get

-- | Empty map.
empty :: HashMap a b
empty = HashMap 0 I.empty

-- | Lookup element in the map.
lookup :: Hash a => a -> HashMap a b -> Maybe b
lookup x (HashMap _ m) = I.lookup (hash x) m >>= find x

-- | Assumption: element is present in the map.
lookupUnsafe :: Hash a => a -> HashMap a b -> b
lookupUnsafe x (HashMap _ m) = fromJust (I.lookup (hash x) m >>= findUnsafe x)

-- | Insert a new element.  The function doesn't check
-- if the element was already present in the map.
insertUnsafe :: Hash a => a -> b -> HashMap a b -> HashMap a b
insertUnsafe x y (HashMap n m) =
    let i = hash x
        f (Just v)  = embed x y v
        f Nothing   = Single x y
    in  HashMap (n + 1) $ I.alter (Just . f) i m

-- | Assumption: element is present in the map.
deleteUnsafe :: Hash a => a -> HashMap a b -> HashMap a b
deleteUnsafe x (HashMap n m) =
    HashMap (n - 1) $ I.update (ejectUnsafe x) (hash x) m
