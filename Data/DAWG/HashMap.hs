{-# LANGUAGE RecordWildCards #-}

module Data.DAWG.HashMap
( Hash (..)
, HashMap
, empty
, size
, lookup
, unsafeInsert
, unsafeLookup
, unsafeDelete
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Data.Function (on)
import Data.Binary (Binary, put, get)
import qualified Data.List as L
import qualified Data.Map as M

fromJust :: Maybe a -> a
fromJust (Just x)   = x
fromJust Nothing    = error "fromJust: Nothing"
{-# INLINE fromJust #-}

class Ord a => Hash a where
    hash    :: a -> Int

data HashMap a b = HashMap
    { size      :: Int
    , hashMap   :: M.Map Int [(a, b)] }
    deriving (Show, Eq, Ord)

instance (Binary a, Binary b) => Binary (HashMap a b) where
    put HashMap{..} = put size >> put hashMap
    get = HashMap <$> get <*> get

-- | Empty map.
empty :: HashMap a b
empty = HashMap 0 M.empty

-- | Lookup element in the map.
lookup :: Hash a => a -> HashMap a b -> Maybe b
lookup x (HashMap _ m) = do
    xs <- M.lookup (hash x) m
    snd <$> L.find ((==x) . fst) xs

-- | Insert the /new/ element.  The function doesn't check
-- if the element was already present in the map.
unsafeInsert :: Hash a => a -> b -> HashMap a b -> HashMap a b
unsafeInsert x y (HashMap n m) =
    let i = hash x
        f (Just xs) = (x, y) : xs
        f Nothing   = [(x, y)]
    in  HashMap (n + 1) $ M.alter (Just . f) i m

-- | Assumption: element is present in the map.
unsafeLookup :: Hash a => a -> HashMap a b -> b
unsafeLookup x (HashMap _ m) = fromJust $ do
    r <- M.lookup (hash x) m
    case r of
        (y:_)   -> return (snd y)
        ys      -> snd <$> L.find ((==x) . fst) ys

-- | Assumption: element is present in the map.
unsafeDelete :: Hash a => a -> HashMap a b -> HashMap a b
unsafeDelete x (HashMap n m) =
    let i = hash x
        f [_]   = Nothing
        f ys    = Just $ L.deleteBy ((==) `on` fst) (x, undefined) ys
    in  HashMap (n - 1) $ M.update f i m
