{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Transition map with a hash.

module Data.DAWG.Trans.Hashed
( Hashed (..)
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Data.DAWG.Util (combine)
import Data.Binary (Binary, put, get)
import Data.DAWG.Trans
import qualified Data.DAWG.Trans.Map as M
import qualified Data.DAWG.Trans.Vector as V

-- | Hash of a transition map is a sum of element-wise hashes.
-- Hash for a given element @(Sym, ID)@ is equal to @combine Sym ID@.
data Hashed t = Hashed
    { hash  :: {-# UNPACK #-} !Int
    , trans :: !t }
    deriving (Show)

instance Binary t => Binary (Hashed t) where
    put Hashed{..} = put hash >> put trans
    get = Hashed <$> get <*> get

instance Trans t => Trans (Hashed t) where
    empty       = Hashed 0 empty
    {-# INLINE empty #-} 

    lookup x    = lookup x . trans
    {-# INLINE lookup #-} 

    index x     = index x . trans
    {-# INLINE index #-} 

    byIndex i   = byIndex i . trans
    {-# INLINE byIndex #-} 

    insert x y (Hashed h t) = Hashed
        (h - h' + combine x y)
        (insert x y t)
      where
        h' = case lookup x t of
            Just y' -> combine x y'
            Nothing -> 0
    {-# INLINE insert #-}

    fromList xs = Hashed 
        (sum $ map (uncurry combine) xs)
        (fromList xs)
    {-# INLINE fromList #-}

    toList  = toList . trans
    {-# INLINE toList #-}

deriving instance Eq  (Hashed M.Trans)
deriving instance Ord (Hashed M.Trans)
deriving instance Eq  (Hashed V.Trans)
deriving instance Ord (Hashed V.Trans)
