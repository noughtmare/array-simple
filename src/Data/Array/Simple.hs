{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-stg-final -dsuppress-all -dno-typeable-binds -dno-suppress-type-signatures -ddump-to-file #-}
-- |
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
--                   Alexey Kuleshevich 2020-2022
--                   Aleksey Khudyakov 2020-2022
--                   Andrew Lelechenko 2020-2022
--                   Jaro Reinders 2026
-- License     : BSD-3-Clause
--
-- Strict immutable arrays.
module Data.Array.Simple (
  -- * Boxed arrays
  Array (UnsafeArray), 

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  -- * Construction

  -- ** Initialisation
  empty, singleton, replicate, generate, iterateN,

  -- ** Unfolding
  unfoldr, unfoldrN, unfoldrExactN,
  iunfoldrN, iunfoldrExactN,

  -- ** Enumeration
  enumFromN, enumFromStepN, 

  -- ** Concatenation
  (++), concat,

  -- * Modifying arrays

  -- ** Permutations
  reverse, backpermute, unsafeBackpermute,

  -- ** Safe destructive updates
  modify,

  -- * Elementwise operations

  -- ** Mapping
  map, imap, concatMap, iconcatMap,

  -- ** Monadic mapping
  mapM_, imapM_, forM_, iforM_,

  -- * Working with predicates

  -- ** Searching
  elem, notElem, find, findIndex, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',
  foldMap, foldMap',

  -- ** Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, maximumOn,
  minimum, minimumBy, minimumOn,

  -- ** Monadic folds
  foldM, 

  -- * Scans
  prescanl, postscanl, scanl, scanl1, iscanl, 

  -- ** Comparisons
  eqBy, cmpBy,

  -- * Conversions

  -- ** Lists
  toList, fromList, fromListN,

  -- ** Mutable arrays
  unsafeFreeze, construct, unsafeConstruct,

  -- ** Grow arrays
  unsafePetrify, grow,
) where

import qualified Data.Array.Simple.Mutable as M
import qualified Data.Array.Simple.Grow as G

import qualified GHC.Exts as GHC

-- import Control.Monad.Primitive
import qualified GHC.ST as GHC
import Control.Monad.ST

import Prelude
  ( Eq (..), Ord (..), Num (..), Monoid (..), Monad (..), Bool, Ordering(..), Int, Maybe
  , (&&), otherwise, error, Maybe (..), Show (..), IO, Foldable, flip, (||), Bool (..), (&&), not
  , fromIntegral, Functor, Semigroup (..), fst)
import qualified Prelude
import Data.Maybe (maybe)
import qualified Data.Foldable as Foldable
import qualified Unsafe.Coerce
import Data.STRef ( readSTRef )
import Control.Monad (unless, when)

-- | This array type is strict in its elements.
-- In a pinch, you can still store lazy elements by defining your own lazy box type
-- at the cost of one extra indirection:
-- 
-- > data Box a = Box a
--
-- This takes less space than lists if there are 3 elements or more.
-- Asymptotically this takes 1/3 the space of a list.
--
-- This always has 3 fewer words than vectors from the vector package.
--
-- The memory representation of a 'Array' is:
--
-- > ╭─────────────┬───╮  ╭────────┬──────┬────────────╮
-- > │ Constructor │ * ┼─➤│ Header │ Size │ Payload... │
-- > ╰─────────────┴───╯  ╰────────┴──────┴────────────╯
--
-- And its overhead is the following:
--
-- * 'UnsafeArray' constructor: 1 word
-- * Pointer to 'SmallArray#': 1 word
-- * 'SmallArray#' Header: 1 word
-- * 'SmallArray#' Size: 1 word
-- * Payload: 1 word per element
--
-- Where a word is the unit of heap allocation,
-- measuring 8 bytes on 64-bit systems, and 4 bytes on 32-bit systems.
data Array a = UnsafeArray {-# UNPACK #-} !(GHC.SmallArray# a)
  -- See Note [SmallArray vs Array]

-- Note [SmallArray vs Array]
-- --------------------------
-- The difference between the two is that Array contains a "card table"
-- to keep track of mutated pointers in the array (just the top-level)
-- to speed up certain parts of garbage collection.
-- 
-- This is not affected by thunks at all and we expect users to mainly
-- use immutable arrays. Our arrays are only mutable while they are being
-- constructed. So the card table is not of much use to us.
--
-- The card table has overhead: 1 word to store the length and furthermore
-- 1 byte per 128 entries. For large arrays this overhead is negligible, but 
-- we also foresee using our arrays for 0-20 elements.

instance Eq a => Eq (Array a) where
  (==) = eqBy (==)

instance Ord a => Ord (Array a) where
  compare = cmpBy compare

instance Show a => Show (Array a) where
  showsPrec p x = showsPrec p (toList x)

instance Functor Array where
  fmap = map

instance Foldable.Foldable Array where
  foldMap = foldMap
  foldr = foldr
  foldl = foldl
  length = length
  foldl' = foldl'
  foldr' = foldr'
  foldr1 = foldr1
  foldl1 = foldl1
  elem = elem
  maximum = maximum
  minimum = minimum
  sum = sum
  product = product
  toList = toList
  null = null

-- Length information
-- ------------------

-- | /O(1)/ Yield the length of the array.
length :: Array a -> Int
{-# INLINE length #-}
length (UnsafeArray arr) = GHC.I# (GHC.sizeofSmallArray# arr)

-- | /O(1)/ Test whether a array is empty.
null :: Array a -> Bool
{-# INLINE null #-}
null v = length v == 0

-- Indexing
-- --------

-- | O(1) Indexing.
(!) :: Array a -> Int -> a
{-# INLINE (!) #-}
v ! i
  | 0 <= i && i < length v = unsafeIndex v i
  | otherwise = error ("!: index out of bounds")

-- | O(1) Safe indexing.
(!?) :: Array a -> Int -> Maybe a
{-# INLINE (!?) #-}
v !? i
  | 0 <= i && i < length v = let !x = unsafeIndex v i in Just x
  | otherwise = Nothing

-- | /O(1)/ First element.
head :: Array a -> a
{-# INLINE head #-}
head v = v ! 0

-- | /O(1)/ Last element.
last :: Array a -> a
{-# INLINE last #-}
last v = v ! (length v - 1)

-- | /O(1)/ Unsafe indexing without bounds checking.
unsafeIndex :: Array a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex (UnsafeArray arr) (GHC.I# i) = let (# !x #) = GHC.indexSmallArray# arr i in x

-- | /O(1)/ First element, without checking if the array is empty.
unsafeHead :: Array a -> a
{-# INLINE unsafeHead #-}
unsafeHead v = unsafeIndex v 0

-- | /O(1)/ Last element, without checking if the array is empty.
unsafeLast :: Array a -> a
{-# INLINE unsafeLast #-}
unsafeLast v = unsafeIndex v (length v - 1)

-- Initialisation
-- --------------

-- | /O(1)/ The empty array.
empty :: Array a
{-# INLINE empty #-}
empty = replicate 0 (Unsafe.Coerce.unsafeCoerce ())

-- | /O(1)/ A array with exactly one element.
singleton :: a -> Array a
{-# INLINE singleton #-}
singleton = replicate 1

-- | /O(n)/ A array of the given length with the same value in each position.
replicate :: Int -> a -> Array a
{-# INLINE replicate #-}
replicate n x = construct n x (\_ -> return ())

-- | /O(n)/ Construct a array of the given length by applying the function to
-- each index.
generate :: Int -> (Int -> a) -> Array a
{-# INLINE generate #-}
generate n f = iunfoldrExactN n (\i _ -> let !x = f i in (x, ())) ()

-- | /O(n)/ Apply the function \(\max(n - 1, 0)\) times to an initial value, producing a array
-- of length \(\max(n, 0)\). The 0th element will contain the initial value, which is why there
-- is one less function application than the number of elements in the produced array.
--
-- \( \underbrace{x, f (x), f (f (x)), \ldots}_{\max(0,n)\rm{~elements}} \)
--
-- ===__Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.iterateN 0 undefined undefined :: V.Array String
-- []
-- >>> V.iterateN 4 (\x -> x <> x) "Hi"
-- ["Hi","HiHi","HiHiHiHi","HiHiHiHiHiHiHiHi"]
iterateN :: Int -> (a -> a) -> a -> Array a
{-# INLINE iterateN #-}
iterateN n f x0 = unfoldrExactN n (\x -> (x, f x)) x0

-- -- Unfolding
-- -- ---------

-- | /O(n)/ Construct a array by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
unfoldr :: (b -> Maybe (a, b)) -> b -> Array a
{-# INLINE unfoldr #-}
unfoldr f !x0 = grow (\g -> do
  let
    go !s =
        case f s of
          Just (x, s') -> do 
            G.pushBack g x
            go s'
          Nothing -> return ()
  go x0)

-- | /O(n)/ Construct a array with at most @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Array a
{-# INLINE unfoldrN #-}
unfoldrN n f x = iunfoldrN n (\_ -> f) x

-- | /O(n)/ Construct a array with exactly @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields the
-- next element and the new seed.
--
-- > unfoldrExactN 3 (\n -> (n,n-1)) 10 = <10,9,8>
unfoldrExactN  :: Int -> (b -> (a, b)) -> b -> Array a
{-# INLINE unfoldrExactN #-}
unfoldrExactN n f x0 = unfoldrN n (\x -> let !y = f x in Just y) x0

-- | /O(n)/ Construct a array with at most @n@ elements by repeatedly applying
-- the generator function to the current index and a seed. The generator 
-- function yields 'Just' the next element and the new seed or 'Nothing' if 
-- there are no more elements.
iunfoldrN :: Int -> (Int -> b -> Maybe (a, b)) -> b -> Array a
{-# INLINE iunfoldrN #-}
iunfoldrN n f !x0 = unsafeConstruct n (\m -> do
  let
    go i !s
      | i < n =
        case f i s of
          Just (x, s') -> do 
            M.unsafeWrite m i x
            go (i + 1) s'
          Nothing -> M.unsafeShrink m i
      | otherwise = return ()
  go 0 x0)

-- | /O(n)/ Construct a array with exactly @n@ elements by repeatedly applying
-- the generator function to the current index and a seed. The generator
-- function yields the next element and the new seed.
iunfoldrExactN :: Int -> (Int -> b -> (a, b)) -> b -> Array a
{-# INLINE iunfoldrExactN #-}
iunfoldrExactN n f x0 = iunfoldrN n (\i x -> Just (f i x)) x0

-- -- Enumeration
-- -- -----------

-- | /O(n)/ Yield a array of the given length, containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: Num a => a -> Int -> Array a
{-# INLINE enumFromN #-}
enumFromN x0 n = generate n (\i -> x0 + fromIntegral i)

-- | /O(n)/ Yield a array of the given length, containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 2 5 = <1,3,5,7,9>
enumFromStepN :: Num a => a -> a -> Int -> Array a
{-# INLINE enumFromStepN #-}
enumFromStepN x0 y n = generate n (\i -> x0 + y * fromIntegral i)

-- -- Concatenation
-- -- -------------

infixr 5 ++
-- | /O(m+n)/ Concatenate two arrays.
(++) :: Array a -> Array a -> Array a
{-# INLINE (++) #-}
v ++ w = generate (length v + length w) (\i -> if i < length v then unsafeIndex v i else unsafeIndex w i)

-- | /O(n)/ Concatenate all arrays in the list.
-- TODO: this could probably be done in a fusible way
concat :: [Array a] -> Array a
{-# INLINE concat #-}
concat [] = empty
concat (v0:vs0) = unfoldrExactN (Prelude.sum (Prelude.map length (v0:vs0))) step (0, v0, vs0) where
  step (!i, !v, !vs) 
      | i < length v = let !x = unsafeIndex v i
                           !i' = i + 1
                       in (x, (i', v, vs))
      | otherwise = let !(v':vs') = vs in step (0, v', vs')

-- Permutations
-- ------------

-- | /O(n)/ Reverse a array.
reverse :: Array a -> Array a
{-# INLINE reverse #-}
reverse v = generate (length v) (\i -> unsafeIndex v (length v - 1 - i))

-- | /O(n)/ Yield the array obtained by replacing each element @i@ of the
-- index array by @xs'!'i@. This is equivalent to @'map' (xs'!') is@, but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: Array a -> Array Int -> Array a
{-# INLINE backpermute #-}
backpermute vx vi = generate (length vi) (\i -> vx ! unsafeIndex vi i)

-- | Same as 'backpermute', but without bounds checking.
unsafeBackpermute :: Array a -> Array Int -> Array a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute vx vi = generate (length vi) (\i -> unsafeIndex vx (unsafeIndex vi i))

-- Safe destructive updates
-- ------------------------

-- | Apply a destructive operation to a array. Only efficient for bulk updates.
modify :: (forall s. M.STArray s a -> ST s ()) -> Array a -> Array a
{-# INLINE modify #-}
modify p v@(UnsafeArray arr) = 
  case length v of
    n@(GHC.I# n#) ->
      unsafeConstruct n (\ m@(M.UnsafeSTArray marr) -> do
      GHC.ST (\s -> (# GHC.copySmallArray# arr 0# marr 0# n# s, () #))
      p m)

-- Mapping
-- -------

-- | /O(n)/ Map a function over a array.
-- Warning: does not fuse, this will allocate a new copy of the array.
-- Consider using explicit streaming (TODO) if you compose this with other combinators.
map :: (a -> b) -> Array a -> Array b
{-# INLINE map #-}
map f v = imap (\_ -> f) v

-- | /O(n)/ Apply a function to every element of a array and its index.
imap :: (Int -> a -> b) -> Array a -> Array b
{-# INLINE imap #-}
imap f v = generate (length v) (\i -> let !x = unsafeIndex v i in f i x)

-- -- | Map a function over a array and concatenate the results.
concatMap :: (a -> Array b) -> Array a -> Array b
{-# INLINE concatMap #-}
concatMap f v = grow (\g -> mapM_ (\x -> mapM_ (G.pushBack g) (f x)) v)

-- | Map a function to every element of a array and its index, and concatenate the results.
--
-- @since 0.13.3.0
iconcatMap :: (Int -> a -> Array b) -> Array a -> Array b
{-# INLINE iconcatMap #-}
iconcatMap f v = grow (\g -> imapM_ (\i x -> mapM_ (G.pushBack g) (f i x)) v)

-- -- Monadic mapping
-- -- ---------------

-- | /O(n)/ Apply the monadic action to all elements of a array and ignore the
-- results.
mapM_ :: Monad m => (a -> m b) -> Array a -> m ()
{-# INLINE mapM_ #-}
mapM_ f = imapM_ (\_ -> f)

-- | /O(n)/ Apply the monadic action to every element of a array and its
-- index, ignoring the results.
imapM_ :: Monad m => (Int -> a -> m b) -> Array a -> m ()
{-# INLINE imapM_ #-}
imapM_ f = ifoldr (\ !i !x xs -> f i x >> xs) (return ())

-- | /O(n)/ Apply the monadic action to all elements of a array and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: Monad m => Array a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ v f = mapM_ f v

-- | /O(n)/ Apply the monadic action to all elements of the array and their indices
-- and ignore the results. Equivalent to @'flip' 'imapM_'@.
iforM_ :: Monad m => Array a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ v f = imapM_ f v

-- -- Searching
-- -- ---------

infix 4 `elem`
-- | /O(n)/ Check if the array contains an element.
elem :: Eq a => a -> Array a -> Bool
{-# INLINE elem #-}
elem z = foldr (\ x xs -> x == z || xs) False

infix 4 `notElem`
-- | /O(n)/ Check if the array does not contain an element (inverse of 'elem').
notElem :: Eq a => a -> Array a -> Bool
{-# INLINE notElem #-}
notElem z v = not (elem z v)

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: (a -> Bool) -> Array a -> Maybe a
{-# INLINE find #-}
find f = foldr (\x xs -> if f x then Just x else xs) Nothing

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: (a -> Bool) -> Array a -> Maybe Int
{-# INLINE findIndex #-}
findIndex f = ifoldr (\i x xs -> if f x then Just i else xs) Nothing

-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: (a -> Bool) -> Array a -> Array Int
{-# INLINE findIndices #-}
findIndices f v = grow (\g -> imapM_ (\i x -> when (f x) (G.pushBack g i)) v)

-- | /O(n)/ Yield 'Just' the index of the first occurrence of the given element or
-- 'Nothing' if the array does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: Eq a => a -> Array a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex x = findIndex (== x)

-- | /O(n)/ Yield the indices of all occurrences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: Eq a => a -> Array a -> Array Int
{-# INLINE elemIndices #-}
elemIndices x = findIndices (== x)

-- -- Folding
-- -- -------

-- | /O(n)/ Left fold.
foldl :: (a -> b -> a) -> a -> Array b -> a
{-# INLINE foldl #-}
foldl k z v = go z 0 where
  go s i
    | i < length v = let !x = unsafeIndex v i in go (k s x) (i + 1)
    | otherwise = s

-- | /O(n)/ Left fold on non-empty arrays.
foldl1 :: (a -> a -> a) -> Array a -> a
{-# INLINE foldl1 #-}
foldl1 k v 
  | null v = error "foldr1 applied to empty array"
  | otherwise = go (unsafeIndex v 0) 1 where
  go s i
    | i < length v - 1 = let !x = unsafeIndex v i in go (k s x) (i + 1)
    | otherwise = unsafeIndex v (length v - 1)

-- | /O(n)/ Left fold with strict accumulator.
foldl' :: (a -> b -> a) -> a -> Array b -> a
{-# INLINE foldl' #-}
foldl' k z = \v -> 
  let
    go !s i
      | i < length v = let !x = unsafeIndex v i in go (k s x) (i + 1)
      | otherwise = s
  in go z 0

-- | /O(n)/ Left fold on non-empty arrays with strict accumulator.
foldl1' :: (a -> a -> a) -> Array a -> a
{-# INLINE foldl1' #-}
foldl1' k v 
  | null v = error "foldr1 applied to empty array"
  | otherwise = go (unsafeIndex v 0) 1 where
  go !s i
    | i < length v - 1 = let !x = unsafeIndex v i in go (k s x) (i + 1)
    | otherwise = unsafeIndex v (length v - 1)

-- | /O(n)/ Right fold.
foldr :: (a -> b -> b) -> b -> Array a -> b
{-# INLINE foldr #-}
foldr k z = \v -> 
  let
    go i
      | i < length v = let !x = unsafeIndex v i in k x (go (i + 1))
      | otherwise = z
  in go 0

-- TODO: implement as left-to-right pass?
-- | /O(n)/ Right fold on non-empty arrays.
foldr1 :: (a -> a -> a) -> Array a -> a
{-# INLINE foldr1 #-}
foldr1 k v 
  | null v = error "foldr1 applied to empty array"
  | otherwise = go (unsafeIndex v (length v - 1)) (length v - 2) where
  go s i
    | 0 <= i = let !x = unsafeIndex v i in go (k x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Right fold with a strict accumulator.
foldr' :: (a -> b -> b) -> b -> Array a -> b
{-# INLINE foldr' #-}
foldr' k z v = go z (length v - 1) where
  go !s i
    | 0 <= i = let !x = unsafeIndex v i in go (k x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Right fold on non-empty arrays with strict accumulator.
foldr1' :: (a -> a -> a) -> Array a -> a
{-# INLINE foldr1' #-}
foldr1' k v 
  | null v = error "foldr1 applied to empty array"
  | otherwise = go (unsafeIndex v (length v - 1)) (length v - 2) where
  go !s i
    | 0 <= i = let !x = unsafeIndex v i in go (k x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Left fold using a function applied to each element and its index.
ifoldl :: (a -> Int -> b -> a) -> a -> Array b -> a
{-# INLINE ifoldl #-}
ifoldl k z v = go z 0 where
  go s i
    | i < length v = let !x = unsafeIndex v i in go (k s i x) (i + 1)
    | otherwise = s

-- | /O(n)/ Left fold with strict accumulator using a function applied to each element
-- and its index.
ifoldl' :: (a -> Int -> b -> a) -> a -> Array b -> a
{-# INLINE ifoldl' #-}
ifoldl' k z v = go z 0 where
  go !s i
    | i < length v = let !x = unsafeIndex v i in go (k s i x) (i + 1)
    | otherwise = s

-- | /O(n)/ Right fold using a function applied to each element and its index.
ifoldr :: (Int -> a -> b -> b) -> b -> Array a -> b
{-# INLINE ifoldr #-}
ifoldr k z v = go 0 where
  go i
    | i < length v = let !x = unsafeIndex v i in k i x (go (i - 1))
    | otherwise = z

-- | /O(n)/ Right fold with strict accumulator using a function applied to each
-- element and its index.
ifoldr' :: (Int -> a -> b -> b) -> b -> Array a -> b
{-# INLINE ifoldr' #-}
ifoldr' k z v = go z (length v - 1) where
  go !s i
    | 0 <= i = let !x = unsafeIndex v i in go (k i x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Map each element of the structure to a monoid and combine
-- the results. It uses the same implementation as the corresponding method
-- of the 'Foldable' type class.
foldMap :: (Monoid m) => (a -> m) -> Array a -> m
{-# INLINE foldMap #-}
foldMap f = foldr (\x m -> f x <> m) mempty

-- | /O(n)/ Like 'foldMap', but strict in the accumulator. It uses the same
-- implementation as the corresponding method of the 'Foldable' type class.
-- Note that it's implemented in terms of 'foldl'', so it fuses in most
-- contGHC.
foldMap' :: (Monoid m) => (a -> m) -> Array a -> m
{-# INLINE foldMap' #-}
foldMap' f = foldr' (\x m -> f x <> m) mempty


-- -- Specialised folds
-- -- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.all even $ V.fromList [2, 4, 12]
-- True
-- >>> V.all even $ V.fromList [2, 4, 13]
-- False
-- >>> V.all even (V.empty :: V.Array Int)
-- True
all :: (a -> Bool) -> Array a -> Bool
{-# INLINE all #-}
all f v = foldr (\x xs -> f x && xs) True v

-- | /O(n)/ Check if any element satisfies the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.any even $ V.fromList [1, 3, 7]
-- False
-- >>> V.any even $ V.fromList [3, 2, 13]
-- True
-- >>> V.any even (V.empty :: V.Array Int)
-- False
any :: (a -> Bool) -> Array a -> Bool
{-# INLINE any #-}
any f = foldr (\x xs -> f x || xs) False

-- | /O(n)/ Check if all elements are 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.and $ V.fromList [True, False]
-- False
-- >>> V.and V.empty
-- True
and :: Array Bool -> Bool
{-# INLINE and #-}
and = foldr (&&) True

-- | /O(n)/ Check if any element is 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.or $ V.fromList [True, False]
-- True
-- >>> V.or V.empty
-- False
or :: Array Bool -> Bool
{-# INLINE or #-}
or = foldr (||) False

-- | /O(n)/ Compute the sum of the elements.
-- Warning: storing numbers (e.g. Int or Double) in a Array
-- is inefficient because of redundant indirections.
-- Consider using unboxed arrays ('Data.Array.Simple.Unboxed.UArray') instead.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.sum $ V.fromList [300,20,1]
-- 321
-- >>> V.sum (V.empty :: V.Array Int)
-- 0
sum :: Num a => Array a -> a
{-# INLINE sum #-}
sum = foldl' (+) 0

-- | /O(n)/ Compute the product of the elements.
-- Warning: storing numbers (e.g. Int or Double) in a Array
-- is inefficient because of redundant indirections.
-- Consider using unboxed arrays ('Data.Array.Simple.Unboxed.UArray') instead.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.product $ V.fromList [1,2,3,4]
-- 24
-- >>> V.product (V.empty :: V.Array Int)
-- 1
product :: Num a => Array a -> a
{-# INLINE product #-}
product = foldl' (*) 1

-- | /O(n)/ Yield the maximum element of the array. The array may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.maximum $ V.fromList [2, 1]
-- 2
-- >>> import Data.Semigroup
-- >>> V.maximum $ V.fromList [Arg 1 'a', Arg 2 'b']
-- Arg 2 'b'
-- >>> V.maximum $ V.fromList [Arg 1 'a', Arg 1 'b']
-- Arg 1 'a'
maximum :: Ord a => Array a -> a
{-# INLINE maximum #-}
maximum = foldl1' max

-- | /O(n)/ Yield the maximum element of the array according to the
-- given comparison function. The array may not be empty. In case of
-- a tie, the first occurrence wins. This behavior is different from
-- 'Data.List.maximumBy' which returns the last tie.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Array as V
-- >>> V.maximumBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- (2,'a')
-- >>> V.maximumBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
maximumBy :: (a -> a -> Ordering) -> Array a -> a
{-# INLINE maximumBy #-}
maximumBy f = foldl1' (\x y -> case f x y of GT -> x; _ -> y)

-- | /O(n)/ Yield the maximum element of the array by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The array may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.maximumOn fst $ V.fromList [(2,'a'), (1,'b')]
-- (2,'a')
-- >>> V.maximumOn fst $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
maximumOn :: Ord b => (a -> b) -> Array a -> a
{-# INLINE maximumOn #-}
maximumOn f v = maybe (error "maximumOn: empty array") fst (foldl' (\s x ->
  case s of
    Just (!y, !fy) ->
      let !fx = f x in if fx > fy then Just (x, fx) else Just (y, fy)
    Nothing -> let !fx = f x in Just (x, fx)
  ) Nothing v)
  

-- | /O(n)/ Yield the minimum element of the array. The array may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.minimum $ V.fromList [2, 1]
-- 1
-- >>> import Data.Semigroup
-- >>> V.minimum $ V.fromList [Arg 2 'a', Arg 1 'b']
-- Arg 1 'b'
-- >>> V.minimum $ V.fromList [Arg 1 'a', Arg 1 'b']
-- Arg 1 'a'
minimum :: Ord a => Array a -> a
{-# INLINE minimum #-}
minimum = foldl1' min

-- | /O(n)/ Yield the minimum element of the array according to the
-- given comparison function. The array may not be empty. In case of
-- a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Array as V
-- >>> V.minimumBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- (1,'b')
-- >>> V.minimumBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
minimumBy :: (a -> a -> Ordering) -> Array a -> a
{-# INLINE minimumBy #-}
minimumBy f = foldl1' (\x y -> case f x y of LT -> x; _ -> y)

-- | /O(n)/ Yield the minimum element of the array by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The array may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.minimumOn fst $ V.fromList [(2,'a'), (1,'b')]
-- (1,'b')
-- >>> V.minimumOn fst $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
minimumOn :: Ord b => (a -> b) -> Array a -> a
{-# INLINE minimumOn #-}
minimumOn f v = maybe (error "minimumOn: empty array") fst (foldl' (\s x ->
  case s of
    Just (!y, !fy) ->
      let !fx = f x in if fx < fy then Just (x, fx) else Just (y, fy)
    Nothing -> let !fx = f x in Just (x, fx)
  ) Nothing v)

-- -- Monadic folds
-- -- -------------

-- | /O(n)/ Monadic fold.
foldM :: Monad m => (a -> b -> m a) -> a -> Array b -> m a
{-# INLINE foldM #-}
-- TODO: this does not generate optimal Core/STG.
-- I guess we'll need to implement these instead of the non-monadic folds.
foldM k z = foldl' (\m y -> do x <- m; k x y) (return z)

-- Scans
-- -----

-- | /O(n)/ Left-to-right prescan (with strict accumulator).
--
-- @
-- prescanl f z = 'init' . 'scanl' f z
-- @
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.prescanl (+) 0 (V.fromList [1,2,3,4])
-- [0,1,3,6]
prescanl :: (a -> b -> a) -> a -> Array b -> Array a
{-# INLINE prescanl #-}
prescanl k z v = iunfoldrExactN (length v) (\i s -> let !x = unsafeIndex v i; s' = k s x in (s, s')) z

-- | /O(n)/ Left-to-right postscan (with strict accumulator).
--
-- @
-- postscanl f z = 'tail' . 'scanl' f z
-- @
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.postscanl (+) 0 (V.fromList [1,2,3,4])
-- [1,3,6,10]
postscanl :: (a -> b -> a) -> a -> Array b -> Array a
{-# INLINE postscanl #-}
postscanl k z v = iunfoldrExactN (length v) (\i s -> let !x = unsafeIndex v i; s' = k s x in (s', s')) z

-- | /O(n)/ Left-to-right scan (with strict accumulator).
--
-- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- >   where y1 = z
-- >         yi = f y(i-1) x(i-1)
--
-- ==== __Examples__
--
-- >>> import qualified Data.Array as V
-- >>> V.scanl (+) 0 (V.fromList [1,2,3,4])
-- [0,1,3,6,10]
scanl :: (a -> b -> a) -> a -> Array b -> Array a
{-# INLINE scanl #-}
scanl k z v = iscanl (\_ -> k) z v

-- | /O(n)/ Left-to-right scan over a array (strictly) with its index.
iscanl :: (Int -> a -> b -> a) -> a -> Array b -> Array a
{-# INLINE iscanl #-}
iscanl k z v = iunfoldrExactN (length v + 1) (\i s -> if i == length v then (s,s) else let !x = unsafeIndex v i; s' = k i s x in (s, s')) z

-- | /O(n)/ Initial-value free left-to-right scan over a array with a strict accumulator.
--
-- ==== __Examples__
-- >>> import qualified Data.Array as V
-- >>> V.scanl1 min $ V.fromListN 5 [4,2,4,1,3]
-- [4,2,2,1,1]
-- >>> V.scanl1 max $ V.fromListN 5 [1,3,2,5,4]
-- [1,3,3,5,5]
-- >>> V.scanl1 min (V.empty :: V.Array Int)
-- []
scanl1 :: (a -> a -> a) -> Array a -> Array a
{-# INLINE scanl1 #-}
scanl1 k v = iunfoldrExactN (length v) (\i s ->
  let !x = unsafeIndex v i in
  case s of
    Nothing -> (x,Just x)
    Just y -> let z = k y x in (z, Just z)
  ) Nothing
-- -- Comparisons
-- -- ------------------------

-- | /O(n)/ Check if two arrays are equal using the supplied equality
-- predicate.
eqBy :: (a -> b -> Bool) -> Array a -> Array b -> Bool
{-# INLINE eqBy #-}
eqBy eq v w = ifoldr (\i x xs -> let !y = unsafeIndex w i in eq x y && xs) True v

-- | /O(n)/ Compare two arrays using the supplied comparison function for
-- array elements. Comparison works the same as for lists (lexicographically).
--
-- > cmpBy compare == compare
cmpBy :: (a -> b -> Ordering) -> Array a -> Array b -> Ordering
{-# INLINE cmpBy #-}
cmpBy cmp v w = ifoldr (\i x xs -> let !y = unsafeIndex w i in cmp x y <> xs) EQ v

-- -- Conversions - Lists
-- -- ------------------------

-- | /O(n)/ Convert a array to a list. Can fuse!
toList :: Array a -> [a]
{-# INLINE toList #-}
toList v = GHC.build (\c n ->
  let 
    go i
      | i < length v = let !x = unsafeIndex v i in x `c` go (i + 1)
      | otherwise = n
  in go 0)

-- | /O(n)/ Convert a list to a array. During the operation, the 
-- array’s capacity will be doubling until the list's contents are 
-- in the array.
fromList :: [a] -> Array a
{-# INLINE fromList #-}
fromList xs = grow (\g -> Prelude.mapM_ (G.pushBack g) xs)

-- | /O(n)/ Convert the first @n@ elements of a list to a array. It's
-- expected that the supplied list will be exactly @n@ elements long. As
-- an optimization, this function allocates a buffer for @n@ elements, which
-- could be used for DoS-attacks by exhausting the memory if an attacker controls
-- that parameter.
--
-- Can fuse!
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
fromListN :: Int -> [a] -> Array a
{-# INLINE fromListN #-}
fromListN n xs = unsafeConstruct n (\m -> do
  Prelude.foldr
    (\x go -> GHC.oneShot (\i -> 
      when (i < n) (do M.unsafeWrite m i x; go (i + 1))))
    (\i -> unless (i == n) (M.unsafeShrink m i))
    xs
    0)

-- Conversions
-- -----------

-- | /O(1)/ Unsafely convert a mutable array to an immutable one without
-- copying. The mutable array may not be used after this operation.
unsafeFreeze :: M.STArray s a -> ST s (Array a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze (M.UnsafeSTArray marr) = GHC.ST (\s ->
  case GHC.unsafeFreezeSmallArray# marr s of
    (# s', arr #) -> (# s', UnsafeArray arr #))

-- | Build an array of a given size and initialised with a given element.
construct :: Int -> a -> (forall s. M.STArray s a -> ST s ()) -> Array a
construct n x0 f = runST (do
  m <- M.new n x0
  f m
  unsafeFreeze m)

-- | Build an array of a given size without initialization, the elements of the mutable array should not be read before first writing to them.
unsafeConstruct :: Int -> (forall s. M.STArray s a -> ST s ()) -> Array a
unsafeConstruct n f = runST (do
  m <- M.unsafeNew n
  f m
  unsafeFreeze m)

-- | /O(1)/ Convert a grow array to an immutable array. The grow array must
-- not be used after this.
unsafePetrify :: G.GrowArray s a -> ST s (Array a)
unsafePetrify (G.UnsafeGrowArray ref) = do
  G.UnsafeGrowArray_ n m <- readSTRef ref
  M.unsafeShrink m n
  unsafeFreeze m

-- | Build an array element by element, doubling capacity when necessary and trimming the array at the end.
-- Use 'Data.Array.Simple.Grow.pushBack' to add elements to the resulting array.
grow :: (forall s. G.GrowArray s a -> ST s ()) -> Array a
grow f = runST (do
  g <- G.new
  f g
  unsafePetrify g)
  
-- $setup
-- >>> :set -Wno-type-defaults
-- >>> import Prelude (Char, String, Bool(True, False), min, max, fst, even, undefined, Ord(..), ($), (<>), Num(..))
