{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-stg-final -dsuppress-all -dno-typeable-binds -dno-suppress-type-signatures -ddump-to-file #-}
-- |
-- Module      : Data.Array.Simple
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
--                   Alexey Kuleshevich 2020-2022
--                   Aleksey Khudyakov 2020-2022
--                   Andrew Lelechenko 2020-2022
-- License     : BSD-style
--
-- Maintainer  : Haskell Libraries Team <libraries@haskell.org>
-- Stability   : experimental
-- Portability : non-portable
--
-- A library for strict immutable arrays (that is, polymorphic arrays capable
-- of holding any Haskell value).
module Data.Array.Simple (
  -- * Boxed arrays
  Array, 
  -- MArray,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  -- -- ** Monadic indexing
  -- indexM, headM, lastM,
  -- unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- -- ** Extracting subarrays (slicing)
  -- slice, 
  -- -- init, tail, take, drop, splitAt, uncons, unsnoc,
  -- unsafeSlice, 
  -- -- unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- * Construction

  -- ** Initialisation
  empty, singleton, replicate, generate, iterateN,

  -- -- ** Monadic initialisation
  -- replicateST, generateST, iterateNST, 
  -- -- create, createT,

  -- ** Unfolding
  -- -- unfoldr, 
  unfoldrN, unfoldrExactN,
  iunfoldrN, iunfoldrExactN,
  -- -- unfoldrM, 
  -- unfoldrNST, unfoldrExactNST,
  -- -- constructN, constructrN,

  -- ** Enumeration
  enumFromN, enumFromStepN, 
  -- enumFromTo, enumFromThenTo,

  -- ** Concatenation
  -- -- cons, snoc, 
  (++), concat,

  -- -- -- ** Restricting memory usage
  -- -- force,

  -- -- * Modifying arrays

  -- -- ** Bulk updates
  -- (//), update, update_,
  -- unsafeUpd, unsafeUpdate, unsafeUpdate_,

  -- -- ** Accumulations
  -- accum, accumulate, accumulate_,
  -- unsafeAccum, unsafeAccumulate, unsafeAccumulate_,

  -- ** Permutations
  reverse, backpermute, unsafeBackpermute,

  -- -- ** Safe destructive updates
  -- -- modify,

  -- -- * Elementwise operations

  -- -- ** Indexing
  -- indexed,

  -- ** Mapping
  map, imap, 
  -- concatMap, iconcatMap,

  -- ** Monadic mapping
  -- mapM, imapM, 
  mapM_, imapM_, 
  -- forM, iforM,
  forM_, iforM_,

  -- -- ** Zipping
  -- zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  -- izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  -- zip, zip3, zip4, zip5, zip6,

  -- -- ** Monadic zipping
  -- zipWithM, izipWithM, zipWithM_, izipWithM_,

  -- -- ** Unzipping
  -- unzip, unzip3, unzip4, unzip5, unzip6,

  -- -- * Working with predicates

  -- -- ** Filtering
  -- filter, ifilter, filterM, uniq,
  -- mapMaybe, imapMaybe,
  -- mapMaybeM, imapMaybeM,
  -- catMaybes,
  -- takeWhile, dropWhile,

  -- -- ** Partitioning
  -- partition, unstablePartition, partitionWith, span, break, spanR, breakR, groupBy, group,

  -- ** Searching
  elem, notElem, find, findIndex, 
  -- findIndexR, findIndices, 
  elemIndex, 
  -- elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',
  foldMap, foldMap',

  -- ** Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, maximumOn,
  minimum, minimumBy, minimumOn,
  -- minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- ** Monadic folds
  foldM, 
  -- ifoldM, foldM', ifoldM',
  -- fold1M, fold1M',foldM_, ifoldM_,
  -- foldM'_, ifoldM'_, fold1M_, fold1M'_,

  -- -- ** Monadic sequencing
  -- sequence, sequence_,

  -- * Scans
  prescanl, postscanl, scanl, scanl1, iscanl, 
  -- prescanr, prescanr',
  -- postscanr, postscanr',
  -- scanr, scanr', scanr1, scanr1',
  -- iscanr, iscanr',

  -- -- * Applicative API
  -- replicateA, generateA, traverse, itraverse, forA, iforA,
  -- traverse_, itraverse_, forA_, iforA_,

  -- ** Comparisons
  eqBy, cmpBy,

  -- * Conversions

  -- ** Lists
  toList, fromList, fromListN,

  -- -- ** Arrays
  -- toArray, fromArray, toArraySlice, unsafeFromArraySlice,

  -- -- ** Other array types
  -- G.convert,

  -- ** Mutable arrays
  -- freeze, 
  unsafeFreeze,
  -- thaw, copy, unsafeCopy,
  -- unsafeThaw, 

  -- ** Grow arrays
  -- petrify, 
  unsafePetrify,

  -- * Slicing (unstable)
  -- ArraySlice (..), whole, unsafeTakeL, unsafeTakeR, unsafeDropL, unsafeDropR,
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

-- Extracting subarrays (slicing)
-- -------------------------------

-- -- | /O(n)/ Yield a slice of the array by copying it. The array must
-- -- contain at least @i+n@ elements.
-- slice :: Int   -- ^ @i@ starting index
--       -> Int   -- ^ @n@ length
--       -> Array a
--       -> Array a
-- {-# INLINE slice #-}
-- slice i n v
--   | 0 <= i && 0 < n && i + n < length v = unsafeSlice i n v
--   | otherwise = error ("Slice arguments out of bounds: " <> show (i, n))

-- -- | /O(1)/ Yield a slice of the array without copying. The array must
-- -- contain at least @i+n@ elements, but this is not checked.
-- unsafeSlice :: Int   -- ^ @i@ starting index
--             -> Int   -- ^ @n@ length
--             -> Array a
--             -> Array a
-- unsafeSlice i n v = runST (do
--   m <- M.unsafeNew n
--   unsafeCopy (unsafeDropL i (whole v)) (M.whole m)
--   unsafeFreeze m)

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
replicate n x = runST (do m <- M.new n x; unsafeFreeze m)

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

-- TODO: this can be implemented when we have growable arrays in the future:
-- -- | /O(n)/ Construct a array by repeatedly applying the generator function
-- -- to a seed. The generator function yields 'Just' the next element and the
-- -- new seed or 'Nothing' if there are no more elements.
-- --
-- -- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- -- >  = <10,9,8,7,6,5,4,3,2,1>
-- unfoldr :: (b -> Maybe (a, b)) -> b -> Array a
-- {-# INLINE unfoldr #-}
-- unfoldr 

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

-- -- -- | /O(n)/ Construct a array by repeatedly applying the monadic
-- -- -- generator function to a seed. The generator function yields 'Just'
-- -- -- the next element and the new seed or 'Nothing' if there are no more
-- -- -- elements.
-- -- unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m (Array a)
-- -- {-# INLINE unfoldrM #-}
-- -- unfoldrM = G.unfoldrM

-- -- | /O(n)/ Construct a array by repeatedly applying the monadic
-- -- generator function to a seed. The generator function yields 'Just'
-- -- the next element and the new seed or 'Nothing' if there are no more
-- -- elements.
-- unfoldrNST :: Int -> (b -> ST s (Maybe (a, b))) -> b -> ST s (Array a)
-- {-# INLINE unfoldrNST #-}
-- unfoldrNST n f x = iunfoldrNST n (\_ -> f) x

-- data While s m b = forall a. MkWhile (ST s a) (a -> m (ST s (Either b a))) 

-- whileST :: Monad m => (forall s. While s m b) -> m b
-- {-# INLINE whileST #-}
-- whileST (MkWhile (GHC.ST x0) step) = GHC.runRW# (\s -> case x0 s of (# s', x0' #) -> go x0' s') where
--   go x s = do
--     GHC.ST x' <- step x
--     case x' s of
--       (# _, Left x'' #) -> return x''
--       (# s', Right x'' #) -> go x'' s'

-- data Generate s f a = forall b c. MkGen (ST s (b, Int, c)) (Int -> b -> f (ST s (c -> c))) (b -> c -> ST s a)

-- generateST :: Applicative f => (forall s. Generate s f a) -> f a
-- generateST (MkGen (GHC.ST start) step end) = GHC.runRW# (\s0 -> do
--   case start s0 of { (# s', (x, n, y) #) -> do
--   let
--     go i = _ (step i x)
--   go 0
--   case end x of { GHC.ST t -> 
--   _
--   }})

-- | /O(n)/ Construct a array with at most @n@ elements by repeatedly applying
-- the generator function to the current index and a seed. The generator 
-- function yields 'Just' the next element and the new seed or 'Nothing' if 
-- there are no more elements.
iunfoldrN :: Int -> (Int -> b -> Maybe (a, b)) -> b -> Array a
{-# INLINE iunfoldrN #-}
iunfoldrN n f !x0 = runST (do
  m <- M.unsafeNew n
  let
    go i x
      | i < n =
        case f i x of
          Just (!y, !x') -> do 
            M.unsafeWrite m i y
            go (i + 1) x'
          Nothing -> do
            M.unsafeShrink m i
            unsafeFreeze m
      | otherwise = unsafeFreeze m
  go 0 x0)

-- | /O(n)/ Construct a array with exactly @n@ elements by repeatedly applying
-- the generator function to the current index and a seed. The generator
-- function yields the next element and the new seed.
iunfoldrExactN :: Int -> (Int -> b -> (a, b)) -> b -> Array a
{-# INLINE iunfoldrExactN #-}
iunfoldrExactN n f x0 = iunfoldrN n (\i x -> Just (f i x)) x0

-- -- | /O(n)/ Construct a array with exactly @n@ elements by repeatedly
-- -- applying the monadic generator function to a seed. The generator
-- -- function yields the next element and the new seed.
-- --
-- -- @since 0.12.2.0
-- unfoldrExactNST :: Int -> (b -> ST s (a, b)) -> b -> ST s (Array a)
-- {-# INLINE unfoldrExactNST #-}
-- unfoldrExactNST n f x0 = unfoldrNST n (\x -> do y <- f x; return (Just y)) x0

-- -- This rule makes sure that fmap Just above gets optimized properly after unfoldrNM 
-- -- is inlined:
-- {-# RULES
-- "fmap/>>=" forall f x k. fmap f x >>= k = x >>= \x' -> k (f x')
-- ">>=/>>=" forall x y z. (x >>= y) >>= z = x >>= \x' -> y x' >>= z
-- ">>=/return" forall x. x >>= return = x
-- "return/>>=" forall x f. return x >>= f = f x
-- ">>=/pure" forall x. x >>= pure = x
-- "pure/>>=" forall x k. pure x >>= k = k x
-- #-}

-- -- | /O(n)/ Construct a array with @n@ elements by repeatedly applying the
-- -- generator function to the already constructed part of the array.
-- --
-- -- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in <a,b,c>
-- constructN :: Int -> (Array a -> a) -> Array a
-- {-# INLINE constructN #-}
-- constructN = G.constructN

-- -- | /O(n)/ Construct a array with @n@ elements from right to left by
-- -- repeatedly applying the generator function to the already constructed part
-- -- of the array.
-- --
-- -- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in <c,b,a>
-- constructrN :: Int -> (Array a -> a) -> Array a
-- {-# INLINE constructrN #-}
-- constructrN = G.constructrN

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

-- -- | /O(n)/ Enumerate values from @x@ to @y@.
-- --
-- -- /WARNING:/ This operation can be very inefficient. If possible, use
-- -- 'enumFromN' instead.
-- enumFromTo :: Enum a => a -> a -> Array a
-- {-# INLINE enumFromTo #-}
-- enumFromTo = G.enumFromTo

-- -- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
-- --
-- -- /WARNING:/ This operation can be very inefficient. If possible, use
-- -- 'enumFromStepN' instead.
-- enumFromThenTo :: Enum a => a -> a -> a -> Array a
-- {-# INLINE enumFromThenTo #-}
-- enumFromThenTo = G.enumFromThenTo

-- -- Concatenation
-- -- -------------

-- -- | /O(n)/ Prepend an element.
-- cons :: a -> Array a -> Array a
-- {-# INLINE cons #-}
-- cons = G.cons

-- -- | /O(n)/ Append an element.
-- snoc :: Array a -> a -> Array a
-- {-# INLINE snoc #-}
-- snoc = G.snoc

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

-- -- -- Monadic initialisation
-- -- -- ----------------------

-- -- | /O(n)/ Execute the monadic action the given number of times and store the
-- -- results in a array.
-- replicateM :: Monad m => Int -> m a -> m (Array a)
-- {-# INLINE replicateM #-}
-- replicateM n m = unfoldrExactNM n (\() -> do x <- m; return (x, ())) ()
-- {-# SPECIALIZE replicateM :: Int -> IO a -> IO (Array a) #-}

-- -- | /O(n)/ Construct a array of the given length by applying the monadic
-- -- action to each index.
-- generateM :: Monad m => Int -> (Int -> m a) -> m (Array a)
-- {-# INLINE generateM #-}
-- generateM n f = iunfoldrNM n (\i () -> do x <- f i; return (Just (x, ()))) ()
-- {-# SPECIALIZE generateM :: Int -> (Int -> IO a) -> IO (Array a) #-}

-- -- | /O(n)/ Apply the monadic function \(\max(n - 1, 0)\) times to an initial value, producing a array
-- -- of length \(\max(n, 0)\). The 0th element will contain the initial value, which is why there
-- -- is one less function application than the number of elements in the produced array.
-- --
-- -- For a non-monadic version, see `iterateN`.
-- --
-- -- @since 0.12.0.0
-- iterateNM :: Monad m => Int -> (a -> m a) -> a -> m (Array a)
-- {-# INLINE iterateNM #-}
-- -- TODO: this doesn't produce optimal Core:
-- iterateNM n f x0 = unfoldrNM n (\ !x -> do x' <- f x; return (x' `seq` Just (x, x'))) x0
-- -- TODO: all monadic functions should have specialize for IO:
-- {-# SPECIALIZE iterateNM :: Int -> (a -> IO a) -> a -> IO (Array a) #-}
-- -- TODO: consider if we really want this:
-- {-# SPECIALIZE iterateNM :: Int -> (a -> Identity a) -> a -> Identity (Array a) #-}

-- -- -- | Execute the monadic action and freeze the resulting array.
-- -- --
-- -- -- @
-- -- -- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- -- -- @
-- -- create :: (forall s. ST s (MArray s a)) -> Array a
-- -- {-# INLINE create #-}
-- -- -- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
-- -- create p = G.create p

-- -- -- | Execute the monadic action and freeze the resulting arrays.
-- -- createT :: Traversable.Traversable f => (forall s. ST s (f (MArray s a))) -> f (Array a)
-- -- {-# INLINE createT #-}
-- -- createT p = G.createT p



-- -- -- Restricting memory usage
-- -- -- ------------------------

-- -- -- | /O(n)/ Yield the argument, but force it not to retain any extra memory,
-- -- -- by copying it.
-- -- --
-- -- -- This is especially useful when dealing with slices. For example:
-- -- --
-- -- -- > force (slice 0 2 <huge array>)
-- -- --
-- -- -- Here, the slice retains a reference to the huge array. Forcing it creates
-- -- -- a copy of just the elements that belong to the slice and allows the huge
-- -- -- array to be garbage collected.
-- -- force :: Array a -> Array a
-- -- {-# INLINE force #-}
-- -- force = G.force

-- -- Bulk updates
-- -- ------------

-- -- | /O(m+n)/ For each pair @(i,a)@ from the list of index/value pairs,
-- -- replace the array element at position @i@ by @a@.
-- --
-- -- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
-- --
-- (//) :: Array a   -- ^ initial array (of length @m@)
--                 -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
--                 -> Array a
-- {-# INLINE (//) #-}
-- (//) = (G.//)

-- -- | /O(m+n)/ For each pair @(i,a)@ from the array of index/value pairs,
-- -- replace the array element at position @i@ by @a@.
-- --
-- -- > update <5,9,2,7> <(2,1),(0,3),(2,8)> = <3,9,8,7>
-- --
-- update :: Array a        -- ^ initial array (of length @m@)
--        -> Array (Int, a) -- ^ array of index/value pairs (of length @n@)
--        -> Array a
-- {-# INLINE update #-}
-- update = G.update

-- -- | /O(m+min(n1,n2))/ For each index @i@ from the index array and the
-- -- corresponding value @a@ from the value array, replace the element of the
-- -- initial array at position @i@ by @a@.
-- --
-- -- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
-- --
-- -- The function 'update' provides the same functionality and is usually more
-- -- convenient.
-- --
-- -- @
-- -- update_ xs is ys = 'update' xs ('zip' is ys)
-- -- @
-- update_ :: Array a   -- ^ initial array (of length @m@)
--         -> Array Int -- ^ index array (of length @n1@)
--         -> Array a   -- ^ value array (of length @n2@)
--         -> Array a
-- {-# INLINE update_ #-}
-- update_ = G.update_

-- -- | Same as ('//'), but without bounds checking.
-- unsafeUpd :: Array a -> [(Int, a)] -> Array a
-- {-# INLINE unsafeUpd #-}
-- unsafeUpd = G.unsafeUpd

-- -- | Same as 'update', but without bounds checking.
-- unsafeUpdate :: Array a -> Array (Int, a) -> Array a
-- {-# INLINE unsafeUpdate #-}
-- unsafeUpdate = G.unsafeUpdate

-- -- | Same as 'update_', but without bounds checking.
-- unsafeUpdate_ :: Array a -> Array Int -> Array a -> Array a
-- {-# INLINE unsafeUpdate_ #-}
-- unsafeUpdate_ = G.unsafeUpdate_

-- -- Accumulations
-- -- -------------

-- -- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the array element
-- -- @a@ at position @i@ by @f a b@.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Array as V
-- -- >>> V.accum (+) (V.fromList [1000,2000,3000]) [(2,4),(1,6),(0,3),(1,10)]
-- -- [1003,2016,3004]
-- accum :: (a -> b -> a) -- ^ accumulating function @f@
--       -> Array a      -- ^ initial array (of length @m@)
--       -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
--       -> Array a
-- {-# INLINE accum #-}
-- accum = G.accum

-- -- | /O(m+n)/ For each pair @(i,b)@ from the array of pairs, replace the array
-- -- element @a@ at position @i@ by @f a b@.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Array as V
-- -- >>> V.accumulate (+) (V.fromList [1000,2000,3000]) (V.fromList [(2,4),(1,6),(0,3),(1,10)])
-- -- [1003,2016,3004]
-- accumulate :: (a -> b -> a)  -- ^ accumulating function @f@
--             -> Array a       -- ^ initial array (of length @m@)
--             -> Array (Int,b) -- ^ array of index/value pairs (of length @n@)
--             -> Array a
-- {-# INLINE accumulate #-}
-- accumulate = G.accumulate

-- -- | /O(m+min(n1,n2))/ For each index @i@ from the index array and the
-- -- corresponding value @b@ from the value array,
-- -- replace the element of the initial array at
-- -- position @i@ by @f a b@.
-- --
-- -- > accumulate_ (+) <5,9,2> <2,1,0,1> <4,6,3,7> = <5+3, 9+6+7, 2+4>
-- --
-- -- The function 'accumulate' provides the same functionality and is usually more
-- -- convenient.
-- --
-- -- @
-- -- accumulate_ f as is bs = 'accumulate' f as ('zip' is bs)
-- -- @
-- accumulate_ :: (a -> b -> a) -- ^ accumulating function @f@
--             -> Array a      -- ^ initial array (of length @m@)
--             -> Array Int    -- ^ index array (of length @n1@)
--             -> Array b      -- ^ value array (of length @n2@)
--             -> Array a
-- {-# INLINE accumulate_ #-}
-- accumulate_ = G.accumulate_

-- -- | Same as 'accum', but without bounds checking.
-- unsafeAccum :: (a -> b -> a) -> Array a -> [(Int,b)] -> Array a
-- {-# INLINE unsafeAccum #-}
-- unsafeAccum = G.unsafeAccum

-- -- | Same as 'accumulate', but without bounds checking.
-- unsafeAccumulate :: (a -> b -> a) -> Array a -> Array (Int,b) -> Array a
-- {-# INLINE unsafeAccumulate #-}
-- unsafeAccumulate = G.unsafeAccumulate

-- -- | Same as 'accumulate_', but without bounds checking.
-- unsafeAccumulate_
--   :: (a -> b -> a) -> Array a -> Array Int -> Array b -> Array a
-- {-# INLINE unsafeAccumulate_ #-}
-- unsafeAccumulate_ = G.unsafeAccumulate_

-- -- Permutations
-- -- ------------

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

-- -- Safe destructive updates
-- -- ------------------------

-- -- | Apply a destructive operation to a array. The operation may be
-- -- performed in place if it is safe to do so and will modify a copy of the
-- -- array otherwise (see 'Data.Array.Generic.New.New' for details).
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Array as V
-- -- >>> import qualified Data.Array.Mutable as MV
-- -- >>> V.modify (\v -> MV.write v 0 'x') $ V.replicate 4 'a'
-- -- "xaaa"
-- modify :: (forall s. MArray s a -> ST s ()) -> Array a -> Array a
-- {-# INLINE modify #-}
-- modify p = G.modify p

-- -- Indexing
-- -- --------

-- TODO: this is probably not worth it without fusion:
--
-- -- | /O(n)/ Pair each element in a array with its index.
-- indexed :: Array a -> Array (Int,a)
-- {-# INLINE indexed #-}
-- indexed = G.indexed

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
-- concatMap :: (a -> Array b) -> Array a -> Array b
-- {-# INLINE concatMap #-}
-- concatMap = G.concatMap

-- -- | Map a function to every element of a array and its index, and concatenate the results.
-- --
-- -- @since 0.13.3.0
-- iconcatMap :: (Int -> a -> Array b) -> Array a -> Array b
-- {-# INLINE iconcatMap #-}
-- iconcatMap = G.iconcatMap

-- -- Monadic mapping
-- -- ---------------

-- -- | /O(n)/ Apply the monadic action to all elements of the array, yielding a
-- -- array of results.
-- mapM :: Monad m => (a -> m b) -> Array a -> m (Array b)
-- {-# INLINE mapM #-}
-- mapM f v = _
  
--  -- iunfoldrNM (length v) (\i () -> let !x = unsafeIndex v i in do y <- f x; return (Just (y, ()))) ()
-- {-# SPECIALIZE mapM :: (a -> IO b) -> Array a -> IO (Array b) #-}

-- -- | /O(n)/ Apply the monadic action to every element of a array and its
-- -- index, yielding a array of results.
-- imapM :: Monad m => (Int -> a -> m b) -> Array a -> m (Array b)
-- {-# INLINE imapM #-}
-- imapM f v = iunfoldrNM (length v) (\i () -> let !x = unsafeIndex v i in do y <- f i x; return (Just (y, ()))) ()
-- {-# SPECIALIZE imapM :: (Int -> a -> IO b) -> Array a -> IO (Array b) #-}

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

-- -- Zipping
-- -- -------

-- -- | /O(min(m,n))/ Zip two arrays with the given function.
-- zipWith :: (a -> b -> c) -> Array a -> Array b -> Array c
-- {-# INLINE zipWith #-}
-- zipWith = G.zipWith

-- -- | Zip three arrays with the given function.
-- zipWith3 :: (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
-- {-# INLINE zipWith3 #-}
-- zipWith3 = G.zipWith3

-- zipWith4 :: (a -> b -> c -> d -> e)
--          -> Array a -> Array b -> Array c -> Array d -> Array e
-- {-# INLINE zipWith4 #-}
-- zipWith4 = G.zipWith4

-- zipWith5 :: (a -> b -> c -> d -> e -> f)
--          -> Array a -> Array b -> Array c -> Array d -> Array e
--          -> Array f
-- {-# INLINE zipWith5 #-}
-- zipWith5 = G.zipWith5

-- zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
--          -> Array a -> Array b -> Array c -> Array d -> Array e
--          -> Array f -> Array g
-- {-# INLINE zipWith6 #-}
-- zipWith6 = G.zipWith6

-- -- | /O(min(m,n))/ Zip two arrays with a function that also takes the
-- -- elements' indices.
-- izipWith :: (Int -> a -> b -> c) -> Array a -> Array b -> Array c
-- {-# INLINE izipWith #-}
-- izipWith = G.izipWith

-- -- | Zip three arrays and their indices with the given function.
-- izipWith3 :: (Int -> a -> b -> c -> d)
--           -> Array a -> Array b -> Array c -> Array d
-- {-# INLINE izipWith3 #-}
-- izipWith3 = G.izipWith3

-- izipWith4 :: (Int -> a -> b -> c -> d -> e)
--           -> Array a -> Array b -> Array c -> Array d -> Array e
-- {-# INLINE izipWith4 #-}
-- izipWith4 = G.izipWith4

-- izipWith5 :: (Int -> a -> b -> c -> d -> e -> f)
--           -> Array a -> Array b -> Array c -> Array d -> Array e
--           -> Array f
-- {-# INLINE izipWith5 #-}
-- izipWith5 = G.izipWith5

-- izipWith6 :: (Int -> a -> b -> c -> d -> e -> f -> g)
--           -> Array a -> Array b -> Array c -> Array d -> Array e
--           -> Array f -> Array g
-- {-# INLINE izipWith6 #-}
-- izipWith6 = G.izipWith6

-- -- | /O(min(m,n))/ Zip two arrays.
-- zip :: Array a -> Array b -> Array (a, b)
-- {-# INLINE zip #-}
-- zip = G.zip

-- -- | Zip together three arrays into a array of triples.
-- zip3 :: Array a -> Array b -> Array c -> Array (a, b, c)
-- {-# INLINE zip3 #-}
-- zip3 = G.zip3

-- zip4 :: Array a -> Array b -> Array c -> Array d
--      -> Array (a, b, c, d)
-- {-# INLINE zip4 #-}
-- zip4 = G.zip4

-- zip5 :: Array a -> Array b -> Array c -> Array d -> Array e
--      -> Array (a, b, c, d, e)
-- {-# INLINE zip5 #-}
-- zip5 = G.zip5

-- zip6 :: Array a -> Array b -> Array c -> Array d -> Array e -> Array f
--      -> Array (a, b, c, d, e, f)
-- {-# INLINE zip6 #-}
-- zip6 = G.zip6

-- -- Unzipping
-- -- ---------

-- -- | /O(min(m,n))/ Unzip a array of pairs.
-- unzip :: Array (a, b) -> (Array a, Array b)
-- {-# INLINE unzip #-}
-- unzip = G.unzip

-- unzip3 :: Array (a, b, c) -> (Array a, Array b, Array c)
-- {-# INLINE unzip3 #-}
-- unzip3 = G.unzip3

-- unzip4 :: Array (a, b, c, d) -> (Array a, Array b, Array c, Array d)
-- {-# INLINE unzip4 #-}
-- unzip4 = G.unzip4

-- unzip5 :: Array (a, b, c, d, e)
--        -> (Array a, Array b, Array c, Array d, Array e)
-- {-# INLINE unzip5 #-}
-- unzip5 = G.unzip5

-- unzip6 :: Array (a, b, c, d, e, f)
--        -> (Array a, Array b, Array c, Array d, Array e, Array f)
-- {-# INLINE unzip6 #-}
-- unzip6 = G.unzip6

-- -- Monadic zipping
-- -- ---------------

-- -- | /O(min(m,n))/ Zip the two arrays with the monadic action and yield a
-- -- array of results.
-- zipWithM :: Monad m => (a -> b -> m c) -> Array a -> Array b -> m (Array c)
-- {-# INLINE zipWithM #-}
-- zipWithM = G.zipWithM

-- -- | /O(min(m,n))/ Zip the two arrays with a monadic action that also takes
-- -- the element index and yield a array of results.
-- izipWithM :: Monad m => (Int -> a -> b -> m c) -> Array a -> Array b -> m (Array c)
-- {-# INLINE izipWithM #-}
-- izipWithM = G.izipWithM

-- -- | /O(min(m,n))/ Zip the two arrays with the monadic action and ignore the
-- -- results.
-- zipWithM_ :: Monad m => (a -> b -> m c) -> Array a -> Array b -> m ()
-- {-# INLINE zipWithM_ #-}
-- zipWithM_ = G.zipWithM_

-- -- | /O(min(m,n))/ Zip the two arrays with a monadic action that also takes
-- -- the element index and ignore the results.
-- izipWithM_ :: Monad m => (Int -> a -> b -> m c) -> Array a -> Array b -> m ()
-- {-# INLINE izipWithM_ #-}
-- izipWithM_ = G.izipWithM_

-- -- Filtering
-- -- ---------

-- -- | /O(n)/ Drop all elements that do not satisfy the predicate.
-- filter :: (a -> Bool) -> Array a -> Array a
-- {-# INLINE filter #-}
-- filter = G.filter

-- -- | /O(n)/ Drop all elements that do not satisfy the predicate which is applied to
-- -- the values and their indices.
-- ifilter :: (Int -> a -> Bool) -> Array a -> Array a
-- {-# INLINE ifilter #-}
-- ifilter = G.ifilter

-- -- | /O(n)/ Drop repeated adjacent elements. The first element in each group is returned.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Array as V
-- -- >>> V.uniq $ V.fromList [1,3,3,200,3]
-- -- [1,3,200,3]
-- -- >>> import Data.Semigroup
-- -- >>> V.uniq $ V.fromList [ Arg 1 'a', Arg 1 'b', Arg 1 'c']
-- -- [Arg 1 'a']
-- uniq :: (Eq a) => Array a -> Array a
-- {-# INLINE uniq #-}
-- uniq = G.uniq

-- -- | /O(n)/ Map the values and collect the 'Just' results.
-- mapMaybe :: (a -> Maybe b) -> Array a -> Array b
-- {-# INLINE mapMaybe #-}
-- mapMaybe = G.mapMaybe

-- -- | /O(n)/ Map the indices/values and collect the 'Just' results.
-- imapMaybe :: (Int -> a -> Maybe b) -> Array a -> Array b
-- {-# INLINE imapMaybe #-}
-- imapMaybe = G.imapMaybe

-- -- | /O(n)/ Return a Array of all the 'Just' values.
-- --
-- -- @since 0.12.2.0
-- catMaybes :: Array (Maybe a) -> Array a
-- {-# INLINE catMaybes #-}
-- catMaybes = mapMaybe id

-- -- | /O(n)/ Drop all elements that do not satisfy the monadic predicate.
-- filterM :: Monad m => (a -> m Bool) -> Array a -> m (Array a)
-- {-# INLINE filterM #-}
-- filterM = G.filterM

-- -- | /O(n)/ Apply the monadic function to each element of the array and
-- -- discard elements returning 'Nothing'.
-- --
-- -- @since 0.12.2.0
-- mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Array a -> m (Array b)
-- {-# INLINE mapMaybeM #-}
-- mapMaybeM = G.mapMaybeM

-- -- | /O(n)/ Apply the monadic function to each element of the array and its index.
-- -- Discard elements returning 'Nothing'.
-- --
-- -- @since 0.12.2.0
-- imapMaybeM :: Monad m => (Int -> a -> m (Maybe b)) -> Array a -> m (Array b)
-- {-# INLINE imapMaybeM #-}
-- imapMaybeM = G.imapMaybeM

-- -- | /O(n)/ Yield the longest prefix of elements satisfying the predicate.
-- -- The current implementation is not copy-free, unless the result array is
-- -- fused away.
-- takeWhile :: (a -> Bool) -> Array a -> Array a
-- {-# INLINE takeWhile #-}
-- takeWhile = G.takeWhile

-- -- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- -- without copying.
-- dropWhile :: (a -> Bool) -> Array a -> Array a
-- {-# INLINE dropWhile #-}
-- dropWhile = G.dropWhile

-- -- Parititioning
-- -- -------------

-- -- | /O(n)/ Split the array in two parts, the first one containing those
-- -- elements that satisfy the predicate and the second one those that don't. The
-- -- relative order of the elements is preserved at the cost of a sometimes
-- -- reduced performance compared to 'unstablePartition'.
-- partition :: (a -> Bool) -> Array a -> (Array a, Array a)
-- {-# INLINE partition #-}
-- partition = G.partition

-- -- | /O(n)/ Split the array into two parts, the first one containing the
-- -- @`Left`@ elements and the second containing the @`Right`@ elements.
-- -- The relative order of the elements is preserved.
-- --
-- -- @since 0.12.1.0
-- partitionWith :: (a -> Either b c) -> Array a -> (Array b, Array c)
-- {-# INLINE partitionWith #-}
-- partitionWith = G.partitionWith

-- -- | /O(n)/ Split the array in two parts, the first one containing those
-- -- elements that satisfy the predicate and the second one those that don't.
-- -- The order of the elements is not preserved, but the operation is often
-- -- faster than 'partition'.
-- unstablePartition :: (a -> Bool) -> Array a -> (Array a, Array a)
-- {-# INLINE unstablePartition #-}
-- unstablePartition = G.unstablePartition

-- -- | /O(n)/ Split the array into the longest prefix of elements that satisfy
-- -- the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Array as V
-- -- >>> V.span (<4) $ V.generate 10 id
-- -- ([0,1,2,3],[4,5,6,7,8,9])
-- span :: (a -> Bool) -> Array a -> (Array a, Array a)
-- {-# INLINE span #-}
-- span = G.span

-- -- | /O(n)/ Split the array into the longest prefix of elements that do not
-- -- satisfy the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Array as V
-- -- >>> V.break (>4) $ V.generate 10 id
-- -- ([0,1,2,3,4],[5,6,7,8,9])
-- break :: (a -> Bool) -> Array a -> (Array a, Array a)
-- {-# INLINE break #-}
-- break = G.break

-- -- | /O(n)/ Split the array into the longest prefix of elements that satisfy
-- -- the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Array as V
-- -- >>> V.spanR (>4) $ V.generate 10 id
-- -- ([5,6,7,8,9],[0,1,2,3,4])
-- --
-- -- @since 0.13.2.0
-- spanR :: (a -> Bool) -> Array a -> (Array a, Array a)
-- {-# INLINE spanR #-}
-- spanR = G.spanR

-- -- | /O(n)/ Split the array into the longest prefix of elements that do not
-- -- satisfy the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Array as V
-- -- >>> V.breakR (<5) $ V.generate 10 id
-- -- ([5,6,7,8,9],[0,1,2,3,4])
-- --
-- -- @since 0.13.2.0
-- breakR :: (a -> Bool) -> Array a -> (Array a, Array a)
-- {-# INLINE breakR #-}
-- breakR = G.breakR

-- -- | /O(n)/ Split a array into a list of slices, using a predicate function.
-- --
-- -- The concatenation of this list of slices is equal to the argument array,
-- -- and each slice contains only equal elements, as determined by the equality
-- -- predicate function.
-- --
-- -- Does not fuse.
-- --
-- -- >>> import qualified Data.Array as V
-- -- >>> import           Data.Char (isUpper)
-- -- >>> V.groupBy (\a b -> isUpper a == isUpper b) (V.fromList "Mississippi River")
-- -- ["M","ississippi ","R","iver"]
-- --
-- -- See also 'Data.List.groupBy', 'group'.
-- --
-- -- @since 0.13.0.0
-- groupBy :: (a -> a -> Bool) -> Array a -> [Array a]
-- {-# INLINE groupBy #-}
-- groupBy = G.groupBy

-- -- | /O(n)/ Split a array into a list of slices of the input array.
-- --
-- -- The concatenation of this list of slices is equal to the argument array,
-- -- and each slice contains only equal elements.
-- --
-- -- Does not fuse.
-- --
-- -- This is the equivalent of 'groupBy (==)'.
-- --
-- -- >>> import qualified Data.Array as V
-- -- >>> V.group (V.fromList "Mississippi")
-- -- ["M","i","ss","i","ss","i","pp","i"]
-- --
-- -- See also 'Data.List.group'.
-- --
-- -- @since 0.13.0.0
-- group :: Eq a => Array a -> [Array a]
-- {-# INLINE group #-}
-- group = G.groupBy (==)

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

-- -- | /O(n)/ Yield 'Just' the index of the /last/ element matching the predicate
-- -- or 'Nothing' if no such element exists.
-- --
-- -- Does not fuse.
-- findIndexR :: (a -> Bool) -> Array a -> Maybe Int
-- {-# INLINE findIndexR #-}
-- findIndexR = G.findIndexR

-- -- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- -- order.
-- findIndices :: (a -> Bool) -> Array a -> Array Int
-- {-# INLINE findIndices #-}
-- findIndices = G.findIndices

-- | /O(n)/ Yield 'Just' the index of the first occurrence of the given element or
-- 'Nothing' if the array does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: Eq a => a -> Array a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex x = findIndex (== x)

-- -- | /O(n)/ Yield the indices of all occurrences of the given element in
-- -- ascending order. This is a specialised version of 'findIndices'.
-- elemIndices :: Eq a => a -> Array a -> Array Int
-- {-# INLINE elemIndices #-}
-- elemIndices = G.elemIndices

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
-- Consider using unboxed arrays (TODO) instead.
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
-- Consider using unboxed arrays (TODO) instead.
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

-- -- | /O(n)/ Yield the index of the maximum element of the array. The array
-- -- may not be empty.
-- maxIndex :: Ord a => Array a -> Int
-- {-# INLINE maxIndex #-}
-- maxIndex = G.maxIndex

-- -- | /O(n)/ Yield the index of the maximum element of the array
-- -- according to the given comparison function. The array may not be
-- -- empty. In case of a tie, the first occurrence wins.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import Data.Ord
-- -- >>> import qualified Data.Array as V
-- -- >>> V.maxIndexBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- -- 0
-- -- >>> V.maxIndexBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- -- 0
-- maxIndexBy :: (a -> a -> Ordering) -> Array a -> Int
-- {-# INLINE maxIndexBy #-}
-- maxIndexBy = G.maxIndexBy

-- -- | /O(n)/ Yield the index of the minimum element of the array. The array
-- -- may not be empty.
-- minIndex :: Ord a => Array a -> Int
-- {-# INLINE minIndex #-}
-- minIndex = G.minIndex

-- -- | /O(n)/ Yield the index of the minimum element of the array according to
-- -- the given comparison function. The array may not be empty.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import Data.Ord
-- -- >>> import qualified Data.Array as V
-- -- >>> V.minIndexBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- -- 1
-- -- >>> V.minIndexBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- -- 0
-- minIndexBy :: (a -> a -> Ordering) -> Array a -> Int
-- {-# INLINE minIndexBy #-}
-- minIndexBy = G.minIndexBy

-- -- Monadic folds
-- -- -------------

-- | /O(n)/ Monadic fold.
foldM :: Monad m => (a -> b -> m a) -> a -> Array b -> m a
{-# INLINE foldM #-}
-- TODO: this does not generate optimal Core/STG. i
-- I guess we'll need to implement these instead of the non-monadic folds.
foldM k z = foldl' (\m y -> do x <- m; k x y) (return z)

-- -- | /O(n)/ Monadic fold using a function applied to each element and its index.
-- ifoldM :: Monad m => (a -> Int -> b -> m a) -> a -> Array b -> m a
-- {-# INLINE ifoldM #-}
-- ifoldM = G.ifoldM

-- -- | /O(n)/ Monadic fold over non-empty arrays.
-- fold1M :: Monad m => (a -> a -> m a) -> Array a -> m a
-- {-# INLINE fold1M #-}
-- fold1M = G.fold1M

-- -- | /O(n)/ Monadic fold with strict accumulator.
-- foldM' :: Monad m => (a -> b -> m a) -> a -> Array b -> m a
-- {-# INLINE foldM' #-}
-- foldM' = G.foldM'

-- -- | /O(n)/ Monadic fold with strict accumulator using a function applied to each
-- -- element and its index.
-- ifoldM' :: Monad m => (a -> Int -> b -> m a) -> a -> Array b -> m a
-- {-# INLINE ifoldM' #-}
-- ifoldM' = G.ifoldM'

-- -- | /O(n)/ Monadic fold over non-empty arrays with strict accumulator.
-- fold1M' :: Monad m => (a -> a -> m a) -> Array a -> m a
-- {-# INLINE fold1M' #-}
-- fold1M' = G.fold1M'

-- -- | /O(n)/ Monadic fold that discards the result.
-- foldM_ :: Monad m => (a -> b -> m a) -> a -> Array b -> m ()
-- {-# INLINE foldM_ #-}
-- foldM_ = G.foldM_

-- -- | /O(n)/ Monadic fold that discards the result using a function applied to
-- -- each element and its index.
-- ifoldM_ :: Monad m => (a -> Int -> b -> m a) -> a -> Array b -> m ()
-- {-# INLINE ifoldM_ #-}
-- ifoldM_ = G.ifoldM_

-- -- | /O(n)/ Monadic fold over non-empty arrays that discards the result.
-- fold1M_ :: Monad m => (a -> a -> m a) -> Array a -> m ()
-- {-# INLINE fold1M_ #-}
-- fold1M_ = G.fold1M_

-- -- | /O(n)/ Monadic fold with strict accumulator that discards the result.
-- foldM'_ :: Monad m => (a -> b -> m a) -> a -> Array b -> m ()
-- {-# INLINE foldM'_ #-}
-- foldM'_ = G.foldM'_

-- -- | /O(n)/ Monadic fold with strict accumulator that discards the result
-- -- using a function applied to each element and its index.
-- ifoldM'_ :: Monad m => (a -> Int -> b -> m a) -> a -> Array b -> m ()
-- {-# INLINE ifoldM'_ #-}
-- ifoldM'_ = G.ifoldM'_

-- -- | /O(n)/ Monadic fold over non-empty arrays with strict accumulator
-- -- that discards the result.
-- fold1M'_ :: Monad m => (a -> a -> m a) -> Array a -> m ()
-- {-# INLINE fold1M'_ #-}
-- fold1M'_ = G.fold1M'_

-- -- Monadic sequencing
-- -- ------------------

-- -- -- | Evaluate each action and collect the results.
-- sequence :: Monad m => Array (m a) -> m (Array a)
-- {-# INLINE sequence #-}
-- sequence v = generateM (length v) (\i -> unsafeIndex v i)
-- {-# SPECIALIZE sequence :: Array (IO a) -> IO (Array a) #-}

-- -- | Evaluate each action and discard the results.
-- sequence_ :: Monad m => Array (m a) -> m ()
-- {-# INLINE sequence_ #-}
-- sequence_ = foldr (\m xs -> m >> xs) (return ())
-- {-# SPECIALIZE sequence_ :: Array (IO a) -> IO () #-}

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

-- -- | /O(n)/ Right-to-left prescan.
-- --
-- -- @
-- -- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- -- @
-- prescanr :: (a -> b -> b) -> b -> Array a -> Array b
-- {-# INLINE prescanr #-}
-- prescanr = G.prescanr

-- -- | /O(n)/ Right-to-left prescan with strict accumulator.
-- prescanr' :: (a -> b -> b) -> b -> Array a -> Array b
-- {-# INLINE prescanr' #-}
-- prescanr' = G.prescanr'

-- -- | /O(n)/ Right-to-left postscan.
-- postscanr :: (a -> b -> b) -> b -> Array a -> Array b
-- {-# INLINE postscanr #-}
-- postscanr = G.postscanr

-- -- | /O(n)/ Right-to-left postscan with strict accumulator.
-- postscanr' :: (a -> b -> b) -> b -> Array a -> Array b
-- {-# INLINE postscanr' #-}
-- postscanr' = G.postscanr'

-- -- | /O(n)/ Right-to-left scan.
-- scanr :: (a -> b -> b) -> b -> Array a -> Array b
-- {-# INLINE scanr #-}
-- scanr = G.scanr

-- -- | /O(n)/ Right-to-left scan with strict accumulator.
-- scanr' :: (a -> b -> b) -> b -> Array a -> Array b
-- {-# INLINE scanr' #-}
-- scanr' = G.scanr'

-- -- | /O(n)/ Right-to-left scan over a array with its index.
-- --
-- -- @since 0.12.0.0
-- iscanr :: (Int -> a -> b -> b) -> b -> Array a -> Array b
-- {-# INLINE iscanr #-}
-- iscanr = G.iscanr

-- -- | /O(n)/ Right-to-left scan over a array (strictly) with its index.
-- --
-- -- @since 0.12.0.0
-- iscanr' :: (Int -> a -> b -> b) -> b -> Array a -> Array b
-- {-# INLINE iscanr' #-}
-- iscanr' = G.iscanr'

-- -- | /O(n)/ Right-to-left, initial-value free scan over a array.
-- --
-- -- Note: Since 0.13, application of this to an empty array no longer
-- -- results in an error; instead it produces an empty array.
-- --
-- -- ==== __Examples__
-- -- >>> import qualified Data.Array as V
-- -- >>> V.scanr1 min $ V.fromListN 5 [3,1,4,2,4]
-- -- [1,1,2,2,4]
-- -- >>> V.scanr1 max $ V.fromListN 5 [4,5,2,3,1]
-- -- [5,5,3,3,1]
-- -- >>> V.scanr1 min (V.empty :: V.Array Int)
-- -- []
-- scanr1 :: (a -> a -> a) -> Array a -> Array a
-- {-# INLINE scanr1 #-}
-- scanr1 = G.scanr1

-- -- | /O(n)/ Right-to-left, initial-value free scan over a array with a strict
-- -- accumulator.
-- --
-- -- Note: Since 0.13, application of this to an empty array no longer
-- -- results in an error; instead it produces an empty array.
-- --
-- -- ==== __Examples__
-- -- >>> import qualified Data.Array as V
-- -- >>> V.scanr1' min $ V.fromListN 5 [3,1,4,2,4]
-- -- [1,1,2,2,4]
-- -- >>> V.scanr1' max $ V.fromListN 5 [4,5,2,3,1]
-- -- [5,5,3,3,1]
-- -- >>> V.scanr1' min (V.empty :: V.Array Int)
-- -- []
-- scanr1' :: (a -> a -> a) -> Array a -> Array a
-- {-# INLINE scanr1' #-}
-- scanr1' = G.scanr1'

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
fromList xs = runST (do
  m <- G.new
  Prelude.mapM_ (G.pushBack m) xs
  unsafePetrify m)

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
fromListN n xs = runST (do
  m <- M.unsafeNew n
  Prelude.foldr 
    (\x go -> GHC.oneShot (\i -> 
      if i < n
        then do M.unsafeWrite m i x; go (i + 1)
        else unsafeFreeze m))
    (\i -> if i == n 
      then unsafeFreeze m 
      else do M.unsafeShrink m i; unsafeFreeze m)
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

-- -- | A slice (subarray) of an immutable array. This takes up 2 extra words, so /4 + n/ words total.
-- data ArraySlice a = UnsafeArraySlice {-# UNPACK #-} !Int !Int !(Array a) 

-- -- | Convert a array to a slice which covers the whole array.
-- whole :: Array a -> ArraySlice a
-- whole v = UnsafeArraySlice 0 (length v) v

-- -- | Take a prefix of a slice
-- unsafeTakeL :: Int -> ArraySlice a -> ArraySlice a
-- unsafeTakeL n (UnsafeArraySlice off _ m) = UnsafeArraySlice off n m

-- -- | Take a suffix of a slice
-- unsafeTakeR :: Int -> ArraySlice a -> ArraySlice a
-- unsafeTakeR n (UnsafeArraySlice off len m) = UnsafeArraySlice (off + len - n) n m

-- -- | Remove a prefix of a slice
-- unsafeDropL :: Int -> ArraySlice a -> ArraySlice a
-- unsafeDropL n (UnsafeArraySlice off len m) = UnsafeArraySlice (off + n) (len - n) m

-- -- | Remove a suffix of a slice
-- unsafeDropR :: Int -> ArraySlice a -> ArraySlice a
-- unsafeDropR n (UnsafeArraySlice off len m) = UnsafeArraySlice off (len - n) m

-- -- | /O(n)/ Yield an immutable copy of the mutable array.
-- freeze :: M.STArraySlice s a -> ST s (Array a)
-- {-# INLINE freeze #-}
-- freeze (M.UnsafeSTArraySlice (M.UnsafeSTArray marr) (GHC.I# off) (GHC.I# len)) = GHC.ST (\s ->
--   case GHC.freezeSmallArray# marr off len s of
--     (# s', arr #) -> (# s', UnsafeArray arr #))

-- -- | /O(n)/ Yield a mutable copy of an immutable array.
-- thaw :: ArraySlice a -> ST s (M.STArray s a)
-- {-# INLINE thaw #-}
-- thaw (UnsafeArraySlice (GHC.I# i) (GHC.I# n) (UnsafeArray arr)) = GHC.ST (\s -> 
--   case GHC.thawSmallArray# arr i n s of
--     (# s', marr #) -> (# s', M.UnsafeSTArray marr #))

-- -- | /O(n)/ Copy an immutable array into a mutable one.
-- unsafeCopy :: ArraySlice a -> M.STArraySlice s a -> ST s ()
-- {-# INLINE unsafeCopy #-}
-- unsafeCopy (UnsafeArraySlice (GHC.I# offv) _ (UnsafeArray arr)) (M.UnsafeSTArraySlice (M.UnsafeSTArray marr) (GHC.I# offm) (GHC.I# len)) = GHC.ST (\s -> 
--   (# GHC.copySmallArray# arr offv marr offm len s , () #))

-- -- | /O(n)/ Copy an immutable array into a mutable one. The two arrays must
-- -- have the same length.
-- copy :: ArraySlice a -> M.STArraySlice s a -> ST s ()
-- {-# INLINE copy #-}
-- copy v@(UnsafeArraySlice _ vn _) m@(M.UnsafeSTArraySlice _ mn _)
--   | mn == vn = unsafeCopy v m
--   | otherwise = error "copy: array slices have different lengths"

-- -- | /O(n)/ Yield an immutable copy of a grow array.
-- petrify :: G.GrowArray s a -> ST s (Array a)
-- petrify (G.UnsafeGrowArray ref) = do
--   G.UnsafeGrowArray_ n m <- readSTRef ref
--   freeze (M.unsafeTakeL n (M.whole m))

-- | /O(1)/ Convert a grow array to an immutable array. The grow array must
-- not be used after this.
unsafePetrify :: G.GrowArray s a -> ST s (Array a)
unsafePetrify (G.UnsafeGrowArray ref) = do
  G.UnsafeGrowArray_ n m <- readSTRef ref
  M.unsafeShrink m n
  unsafeFreeze m
  
-- $setup
-- >>> :set -Wno-type-defaults
-- >>> import Prelude (Char, String, Bool(True, False), min, max, fst, even, undefined, Ord(..), ($), (<>), Num(..))
