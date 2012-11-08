{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

-- | A (prefix) trie.

module Data.Trie.Unboxed
( TrieM
, Trie (..)
, empty
, unTrie
, child
, edges
, mkTrie
, setValue
, substChild
, insert

, size
, follow
, lookup
, fromLang
, fromList
, toList

, toDAWG
, toDAWG'With
, fromDAWG
, implicitDAWG

, Anno
, annoTrie
, numAnno
, heightAnno
, hashAnno
, flatten
, Schema (..)
, schemaId
, schemaNum
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Control.Monad ((>=>))
import Data.Ord (comparing)
import Data.List (sortBy, foldl')
import Data.Binary (Binary, get, put)
import Data.Vector.Binary ()
import Data.Vector.Unboxed (Unbox)
import Data.Hashable (Hashable, hash, combine)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import qualified Data.DAWG.Unboxed as D

-- | A 'Trie' with 'Maybe' values in nodes.
type TrieM a b = Trie a (Maybe b)

-- | A trie of words with character type @a@ and entry type @b@.
-- It represents a 'M.Map' from @[a]@ keys to @b@ values.
data Trie a b = Trie {
    -- | Value in the root node.
    rootValue   :: !b,                  
    -- | Edges to substries annotated with 'labels'.
    subTries    :: !(V.Vector (Trie a b)),
    -- | Edge annotations.
    labels      :: !(U.Vector a)
    } deriving (Show, Eq, Ord)

instance Functor (Trie a) where
    fmap f Trie{..} = Trie (f rootValue) (V.map (fmap f) subTries) labels

instance (Unbox a, Binary a, Binary b) => Binary (Trie a b) where
    put Trie{..} = do
        put rootValue
        put subTries
        put labels
    get = Trie <$> get <*> get <*> get

-- | Decompose the trie into a pair of root value and edge list.
unTrie :: Unbox a => Trie a b -> (b, [(a, Trie a b)])
unTrie Trie{..} =
    ( rootValue
    , zip (U.toList labels) (V.toList subTries) )
{-# INLINE unTrie #-}

-- | Return trie edges as a list of (annotation character, subtrie) pairs.
edges :: Unbox a => Trie a b -> [(a, Trie a b)]
edges = snd . unTrie
{-# INLINE edges #-}

-- | Child of the trie following the edge annotated with the given character.
child :: (Eq a, Unbox a) => a -> Trie a b -> Maybe (Trie a b)
child x Trie{..} = case U.elemIndex x labels of
    Just i  -> Just (subTries V.! i)
    Nothing -> Nothing 
{-# INLINE child #-}

children :: Unbox a => Trie a b -> [Trie a b]
children = map snd . edges
{-# INLINE children #-}

-- | Construct trie from the root value and the list of edges.
mkTrie :: Unbox a => b -> [(a, Trie a b)] -> Trie a b
mkTrie !v !cs =
    let (xs, ys) = unzip cs
    in  Trie v (V.fromList ys) (U.fromList xs)
{-# INLINE mkTrie #-}

-- | Empty trie.
empty :: Unbox a => TrieM a b
empty = mkTrie Nothing []
{-# INLINE empty #-}

-- | Set the value in the root of the trie.
setValue :: b -> Trie a b -> Trie a b
setValue !x !t = t { rootValue = x }
{-# INLINE setValue #-}

-- | Substitute subtrie attached to the edge annotated with the given
-- character (or add new edge if such edge doesn't exist).
substChild :: (Eq a, Unbox a) => a -> Trie a b -> Trie a b -> Trie a b
substChild !x !t !newChild = case U.elemIndex x (labels t) of
    Just i  -> t
        { subTries = (V.//) (subTries t) [(i, newChild)] }
    Nothing -> t
        { subTries = V.snoc (subTries t) newChild
        , labels   = U.snoc (labels t) x }
{-# INLINE substChild #-}

-- | Insert word with the given value to the trie.
insert :: (Eq a, Unbox a) => [a] -> b -> TrieM a b -> TrieM a b
insert [] v t = setValue (Just v) t
insert (x:xs) v t = substChild x t . insert xs v $
    case child x t of
        Just t' -> t'
        Nothing -> empty
{-# INLINABLE insert #-}
{-# SPECIALIZE insert
    :: String -> b
    -> TrieM Char b
    -> TrieM Char b #-}

-- | Size of the trie.
size :: Unbox a => Trie a b -> Int
size t = 1 + sum (map (size.snd) (edges t))

-- | Follow the path determined by the input word starting
-- in the trie root.
follow :: (Eq a, Unbox a) => [a] -> Trie a b -> Maybe (Trie a b)
follow xs t = foldr (>=>) return (map child xs) t

-- | Search for the value assigned to the given word in the trie.
lookup :: (Eq a, Unbox a) => [a] -> TrieM a b -> Maybe b
lookup xs t = follow xs t >>= rootValue

-- | Construct the trie from the list of (word, value) pairs.
fromList :: (Eq a, Unbox a) => [([a], b)] -> TrieM a b
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs

-- | Deconstruct the trie into a list of (word, value) pairs.
toList :: Unbox a => TrieM a b -> [([a], b)]
toList t = case rootValue t of
    Just y -> ([], y) : lower
    Nothing -> lower
  where
    lower = concatMap goDown $ edges t
    goDown (x, t') = map (addChar x) $ toList t'
    addChar x (xs, y) = (x:xs, y)

-- | Make the trie from the list of words.  Annotate each word with
-- the @()@ value.
fromLang :: (Eq a, Unbox a) => [[a]] -> TrieM a ()
fromLang xs = fromList [(x, ()) | x <- xs]

-- | Assign unique identifiers to all unique (sub)tries.  The root trie
-- will be assigned 0 identifier.
identify :: (Ord a, Ord b, Unbox a) => Trie a b -> M.Map (Trie a b) Int
identify t =
    let ts  = concatMap generate (children t)
	nub = S.toList . S.fromList
    in  M.fromList (zip (t : nub ts) [0..])

-- | Generate all (sub)tries from the trie.
generate :: Unbox a => Trie a b -> [Trie a b]
generate t = t : concatMap generate (children t)

-- | Construct DAWG from a trie.
toDAWG'With
    :: (Ord a, Ord b, Ord c, Unbox a)
    => Schema a b c -> Trie a b -> D.DAWG a b
toDAWG'With Schema{..} root' =
    mkDAWG (m M.! root) (map snd $ sortBy (comparing fst) xs)
  where
    root = anno root'
    m  = identify root
    xs = [ ( ix, mkNode (ret $ rootValue t)
            [ (c, m M.! s)
            | (c, s) <- edges t ] )
         | (t, ix) <- M.assocs m ]
    mkDAWG i ys = D.DAWG i (V.fromList ys)
    mkNode v ys = D.Node v (U.fromList ys)

-- | Construct DAWG from a trie.
toDAWG :: (Ord a, Ord b, Unbox a) => Trie a b -> D.DAWG a b
toDAWG root =
    mkDAWG (m M.! root) (map snd $ sortBy (comparing fst) xs)
  where
    m  = identify root
    xs = [ ( ix, mkNode (rootValue t)
            [ (c, m M.! s)
            | (c, s) <- edges t ] )
         | (t, ix) <- M.assocs m ]
    mkDAWG i ys = D.DAWG i (V.fromList ys)
    mkNode v ys = D.Node v (U.fromList ys)

-- | Construct trie from the DAWG.
fromDAWG :: Unbox a => D.DAWG a b -> Trie a b
fromDAWG dag =
    fromIx (D.root dag)
  where
    fromIx i = mkTrie
        (D.valueBy dag i)
        (map (second fromIx) (D.edges dag i))

-- | Elminate common subtries.  The result is algebraically a trie
-- but is represented as a DAWG in memory.
implicitDAWG :: (Ord a, Ord b, Unbox a) => Trie a b -> Trie a b
implicitDAWG = fromDAWG . toDAWG

-- | Annotation schema @anno v xs@ takes the root value @v@ of the trie,
-- labels and annotations @xs@ assigned to its subtries and returns annotation
-- which will be assigned to the trie.  The contract of schema is that two
-- equal tries according to the '==' method must be assigned the same
-- annotations.
type Anno a b c = b -> [(a, c)] -> c

-- Use Hashable instances to annotate each node with a hash value.

-- | Annotate trie using the provided schema.
annoTrie :: Unbox a => Anno a b c -> Trie a b -> Trie a (c, b)
annoTrie anno t = Trie
    { rootValue = (lb, rootValue t)
    , subTries  = V.fromList ts
    , labels    = labels t }
  where
    ts = map (annoTrie anno) (children t)
    xs = U.toList (labels t)
    ys = map (fst . rootValue) ts
    lb = anno (rootValue t) (zip xs ys)

-- | Number of nodes.
numAnno :: Anno a b Int
numAnno _ xs = 1 + sum (map snd xs)

-- | Trie height.
heightAnno :: Anno a b Int
heightAnno _ xs = 1 + maximum (map snd xs)

-- | Trie hash.
hashAnno :: (Hashable a, Hashable b) => Anno a b Int
hashAnno v xs =
    let hashIt (x, c) = combine (hash x) c
    in  foldl combine 0 $ hash v : map hashIt xs

-- | Flatten two int annotations using the combine function from
-- the Hashable module.
flatten :: (Int, (Int, a)) -> (Int, a)
flatten (x, (y, v)) = (combine x y, v)

-- | A annotation schema.
data Schema a b c = Schema
    { anno  :: Trie a b -> Trie a c
    , ret   :: c -> b }

schemaId :: Schema a b b
schemaId = Schema id id

schemaNum :: Unbox a => Schema a b (Int, b)
schemaNum = Schema (annoTrie numAnno) snd
