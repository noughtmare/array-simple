{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-stg-final -dsuppress-all -dno-typeable-binds -dno-suppress-type-signatures -ddump-to-file #-}
-- |
-- Module      : Data.Vector
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
-- A library for lazy boxed vectors (that is, polymorphic arrays capable of
-- holding any Haskell value). The vectors come in two flavours:
--
--  * mutable
--
--  * immutable
--
-- They support a rich interface of both list-like operations and bulk
-- array operations.
--
-- For unboxed arrays, use "Data.Vector.Unboxed".

module Data.Vector (
  -- * Boxed vectors
  Vector, 
  -- MVector,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  -- -- ** Monadic indexing
  -- indexM, headM, lastM,
  -- unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- ** Extracting subvectors (slicing)
  slice, 
  -- init, tail, take, drop, splitAt, uncons, unsnoc,
  unsafeSlice, 
  -- unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- * Construction

  -- ** Initialisation
  empty, singleton, replicate, generate, iterateN,

  -- ** Monadic initialisation
  -- replicateST, generateST, iterateNST, 
  -- -- create, createT,

  -- -- ** Unfolding
  -- -- unfoldr, 
  unfoldrN, unfoldrExactN,
  iunfoldrN, iunfoldrExactN,
  -- -- unfoldrM, 
  -- unfoldrNST, unfoldrExactNST,
  -- -- constructN, constructrN,

  -- -- ** Enumeration
  enumFromN, enumFromStepN, 
  -- enumFromTo, enumFromThenTo,

  -- -- ** Concatenation
  -- -- cons, snoc, 
  (++), concat,

  -- -- -- ** Restricting memory usage
  -- -- force,

  -- -- * Modifying vectors

  -- -- ** Bulk updates
  -- (//), update, update_,
  -- unsafeUpd, unsafeUpdate, unsafeUpdate_,

  -- -- ** Accumulations
  -- accum, accumulate, accumulate_,
  -- unsafeAccum, unsafeAccumulate, unsafeAccumulate_,

  -- -- ** Permutations
  reverse, backpermute, unsafeBackpermute,

  -- -- ** Safe destructive updates
  -- -- modify,

  -- -- * Elementwise operations

  -- -- ** Indexing
  -- indexed,

  -- -- ** Mapping
  map, imap, 
  -- concatMap, iconcatMap,

  -- -- ** Monadic mapping
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

  -- -- ** Searching
  elem, notElem, find, findIndex, 
  -- findIndexR, findIndices, 
  elemIndex, 
  -- elemIndices,

  -- -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',
  foldMap, foldMap',

  -- -- ** Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, maximumOn,
  minimum, minimumBy, minimumOn,
  -- minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- -- ** Monadic folds
  foldM, 
  -- ifoldM, foldM', ifoldM',
  -- fold1M, fold1M',foldM_, ifoldM_,
  -- foldM'_, ifoldM'_, fold1M_, fold1M'_,

  -- -- ** Monadic sequencing
  -- sequence, sequence_,

  -- -- * Scans
  -- prescanl, 
  prescanl',
  -- postscanl, 
  postscanl',
  -- scanl, 
  scanl', 
  -- scanl1, 
  scanl1',
  -- iscanl, 
  iscanl',
  -- prescanr, prescanr',
  -- postscanr, postscanr',
  -- scanr, scanr', scanr1, scanr1',
  -- iscanr, iscanr',

  -- -- * Applicative API
  -- replicateA, generateA, traverse, itraverse, forA, iforA,
  -- traverse_, itraverse_, forA_, iforA_,

  -- -- ** Comparisons
  eqBy, cmpBy,

  -- -- * Conversions

  -- -- ** Lists
  toList, 
  -- fromList,
  fromListN,

  -- -- ** Arrays
  -- toArray, fromArray, toArraySlice, unsafeFromArraySlice,

  -- -- ** Other vector types
  -- G.convert,

  -- ** Mutable vectors
  freeze, unsafeFreeze, thaw, copy, unsafeCopy,
  -- unsafeThaw, 

  -- ** Slice
  VectorSlice (..), whole, unsafeTakeL, unsafeTakeR, unsafeDropL, unsafeDropR,
) where

-- import Control.Applicative (Applicative)
-- import Data.Primitive.Array
-- import qualified Data.Traversable as Traversable
-- import Data.Vector.Mutable.Unsafe ( MVector )
-- import Data.Vector.Unsafe
-- import qualified Data.Vector.Generic as G
import qualified Data.Vector.Mutable as M

import qualified GHC.Exts as GHC
import Data.Elevator (Strict (Strict))

-- import Control.Monad.Primitive
import qualified GHC.ST as GHC
import Control.Monad.ST

import Prelude
  ( Eq (..), Ord (..), Num (..), Monoid, Monad (..), Bool, Ordering(..), Int, Maybe
  , (&&), otherwise, error, Maybe (..), Show (..), Either (..), IO)
import qualified Prelude
import Data.Maybe (maybe)
import qualified Data.Foldable as Foldable

-- | This vector type is strict in its elements.
-- if you want to store lazy things inside, you can define your own lazy box type:
-- data Box a = Box a 
data Vector a = UnsafeVector {-# UNPACK #-} !(GHC.SmallArray# (Strict a))

-- Note [SmallArray vs Array]
-- --------------------------
--
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

instance Eq a => Eq (Vector a) where
  (==) = eqBy (==)

instance Ord a => Ord (Vector a) where
  compare = cmpBy compare

instance Show a => Show (Vector a) where
  showsPrec p x = Prelude.showParen (p > 10) (Prelude.showString "fromList " Prelude.. showsPrec 11 (toList x))

instance Prelude.Functor Vector where
  fmap = map

instance Foldable.Foldable Vector where
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

-- -- | /O(1)/ Convert an array to a vector.
-- --
-- -- @since 0.12.2.0
-- fromArray :: Array a -> Vector a
-- {-# INLINE fromArray #-}
-- fromArray arr = UnsafeVector arr

-- -- | /O(n)/ Convert a vector to an array.
-- --
-- -- @since 0.12.2.0
-- toArray :: Vector a -> Array a
-- {-# INLINE toArray #-}
-- toArray (UnsafeVector arr) = arr

-- Length information
-- ------------------

-- | /O(1)/ Yield the length of the vector.
length :: Vector a -> Int
{-# INLINE length #-}
length (UnsafeVector arr) = GHC.I# (GHC.sizeofSmallArray# arr)

-- | /O(1)/ Test whether a vector is empty.
null :: Vector a -> Bool
{-# INLINE null #-}
null v = length v == 0

-- Indexing
-- --------

-- | O(1) Indexing.
(!) :: Vector a -> Int -> a
{-# INLINE (!) #-}
v ! i
  | 0 <= i && i < length v = unsafeIndex v i
  | otherwise = error ("index out of bounds: " Prelude.++ show i)

-- | O(1) Safe indexing.
(!?) :: Vector a -> Int -> Maybe a
{-# INLINE (!?) #-}
v !? i
  | 0 <= i && i < length v = Just (unsafeIndex v i)
  | otherwise = Nothing

-- | /O(1)/ First element.
head :: Vector a -> a
{-# INLINE head #-}
head v = v ! 0

-- | /O(1)/ Last element.
last :: Vector a -> a
{-# INLINE last #-}
last v = v ! (length v - 1)

-- | /O(1)/ Unsafe indexing without bounds checking.
unsafeIndex :: Vector a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex (UnsafeVector arr) (GHC.I# i) = let (# Strict x #) = GHC.indexSmallArray# arr i in x

-- | /O(1)/ First element, without checking if the vector is empty.
unsafeHead :: Vector a -> a
{-# INLINE unsafeHead #-}
unsafeHead v = unsafeIndex v 0

-- | /O(1)/ Last element, without checking if the vector is empty.
unsafeLast :: Vector a -> a
{-# INLINE unsafeLast #-}
unsafeLast v = unsafeIndex v (length v - 1)

-- Extracting subvectors (slicing)
-- -------------------------------

-- | /O(n)/ Yield a slice of the vector by copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Int   -- ^ @i@ starting index
      -> Int   -- ^ @n@ length
      -> Vector a
      -> Vector a
{-# INLINE slice #-}
slice i n v
  | 0 <= i && 0 < n && i + n < length v = unsafeSlice i n v
  | otherwise = error ("Slice arguments out of bounds: " Prelude.++ show (i, n))

-- | /O(1)/ Yield a slice of the vector without copying. The vector must
-- contain at least @i+n@ elements, but this is not checked.
unsafeSlice :: Int   -- ^ @i@ starting index
            -> Int   -- ^ @n@ length
            -> Vector a
            -> Vector a
unsafeSlice i n v = runST (do
  m <- M.unsafeNew n
  unsafeCopy (unsafeDropL i (whole v)) (M.whole m)
  unsafeFreeze m)

-- Initialisation
-- --------------

-- | /O(1)/ The empty vector.
empty :: Vector a
{-# INLINE empty #-}
empty = replicate 0 (GHC.unsafeCoerce# ())

-- | /O(1)/ A vector with exactly one element.
singleton :: a -> Vector a
{-# INLINE singleton #-}
singleton = replicate 1

-- | /O(n)/ A vector of the given length with the same value in each position.
replicate :: Int -> a -> Vector a
{-# INLINE replicate #-}
replicate n x = runST (do m <- M.new n x; unsafeFreeze m)

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index.
generate :: Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate n f = iunfoldrN n (\i () -> Just (f i, ())) ()

-- | /O(n)/ Apply the function \(\max(n - 1, 0)\) times to an initial value, producing a vector
-- of length \(\max(n, 0)\). The 0th element will contain the initial value, which is why there
-- is one less function application than the number of elements in the produced vector.
--
-- \( \underbrace{x, f (x), f (f (x)), \ldots}_{\max(0,n)\rm{~elements}} \)
--
-- ===__Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.iterateN 0 undefined undefined :: V.Vector String
-- []
-- >>> V.iterateN 4 (\x -> x <> x) "Hi"
-- ["Hi","HiHi","HiHiHiHi","HiHiHiHiHiHiHiHi"]
--
-- @since 0.7.1
iterateN :: Int -> (a -> a) -> a -> Vector a
{-# INLINE iterateN #-}
iterateN n f x0 = unfoldrExactN n (\x -> (x, f x)) x0

-- -- Unfolding
-- -- ---------

-- TODO: this can be implemented when we have growable vectors in the future:
-- -- | /O(n)/ Construct a vector by repeatedly applying the generator function
-- -- to a seed. The generator function yields 'Just' the next element and the
-- -- new seed or 'Nothing' if there are no more elements.
-- --
-- -- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- -- >  = <10,9,8,7,6,5,4,3,2,1>
-- unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
-- {-# INLINE unfoldr #-}
-- unfoldr 

-- | /O(n)/ Construct a vector with at most @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldrN #-}
unfoldrN n f x = iunfoldrN n (\_ -> f) x

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields the
-- next element and the new seed.
--
-- > unfoldrExactN 3 (\n -> (n,n-1)) 10 = <10,9,8>
--
-- @since 0.12.2.0
unfoldrExactN  :: Int -> (b -> (a, b)) -> b -> Vector a
{-# INLINE unfoldrExactN #-}
unfoldrExactN n f x = unfoldrN n (Just Prelude.. f) x

-- -- -- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- -- -- generator function to a seed. The generator function yields 'Just'
-- -- -- the next element and the new seed or 'Nothing' if there are no more
-- -- -- elements.
-- -- unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m (Vector a)
-- -- {-# INLINE unfoldrM #-}
-- -- unfoldrM = G.unfoldrM

-- -- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- -- generator function to a seed. The generator function yields 'Just'
-- -- the next element and the new seed or 'Nothing' if there are no more
-- -- elements.
-- unfoldrNST :: Int -> (b -> ST s (Maybe (a, b))) -> b -> ST s (Vector a)
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

-- basically all you need to construct a vector
iunfoldrN :: Int -> (Int -> b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE iunfoldrN #-}
iunfoldrN n f x0 = runST (do
  m <- M.unsafeNew n
  let
    go i x
      | i < n =
        case f i x of
          Just (!y, !x') -> do 
            M.unsafeWrite m i y
            go (i + 1) x'
          Nothing -> freeze (M.unsafeTakeL i (M.whole m))
      | otherwise = unsafeFreeze m
  go 0 x0)

iunfoldrExactN :: Int -> (Int -> b -> (a, b)) -> b -> Vector a
{-# INLINE iunfoldrExactN #-}
iunfoldrExactN n f x0 = iunfoldrN n (\i x -> Just (f i x)) x0

-- -- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly
-- -- applying the monadic generator function to a seed. The generator
-- -- function yields the next element and the new seed.
-- --
-- -- @since 0.12.2.0
-- unfoldrExactNST :: Int -> (b -> ST s (a, b)) -> b -> ST s (Vector a)
-- {-# INLINE unfoldrExactNST #-}
-- unfoldrExactNST n f x0 = unfoldrNST n (\x -> do y <- f x; return (Just y)) x0

-- TODO: SPECIALIZE prevents inlining, reconsider all uses of it.

-- -- This rule makes sure that fmap Just above gets optimized properly after unfoldrNM 
-- -- is inlined:
-- {-# RULES
-- "fmap/>>=" forall f x k. Prelude.fmap f x Prelude.>>= k = x Prelude.>>= \x' -> k (f x')
-- ">>=/>>=" forall x y z. (x Prelude.>>= y) Prelude.>>= z = x Prelude.>>= \x' -> y x' Prelude.>>= z
-- ">>=/return" forall x. x Prelude.>>= Prelude.return = x
-- "return/>>=" forall x f. Prelude.return x Prelude.>>= f = f x
-- ">>=/pure" forall x. x Prelude.>>= Prelude.pure = x
-- "pure/>>=" forall x k. Prelude.pure x Prelude.>>= k = k x
-- #-}

-- -- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- -- generator function to the already constructed part of the vector.
-- --
-- -- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in <a,b,c>
-- constructN :: Int -> (Vector a -> a) -> Vector a
-- {-# INLINE constructN #-}
-- constructN = G.constructN

-- -- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- -- repeatedly applying the generator function to the already constructed part
-- -- of the vector.
-- --
-- -- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in <c,b,a>
-- constructrN :: Int -> (Vector a -> a) -> Vector a
-- {-# INLINE constructrN #-}
-- constructrN = G.constructrN

-- -- Enumeration
-- -- -----------

-- | /O(n)/ Yield a vector of the given length, containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: Num a => a -> Int -> Vector a
{-# INLINE enumFromN #-}
enumFromN x0 n = unfoldrExactN n (\ x -> (x, x + 1)) x0

-- | /O(n)/ Yield a vector of the given length, containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 2 5 = <1,3,5,7,9>
enumFromStepN :: Num a => a -> a -> Int -> Vector a
{-# INLINE enumFromStepN #-}
enumFromStepN x0 y n = unfoldrExactN n (\ x -> (x, x + y)) x0

-- -- | /O(n)/ Enumerate values from @x@ to @y@.
-- --
-- -- /WARNING:/ This operation can be very inefficient. If possible, use
-- -- 'enumFromN' instead.
-- enumFromTo :: Enum a => a -> a -> Vector a
-- {-# INLINE enumFromTo #-}
-- enumFromTo = G.enumFromTo

-- -- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
-- --
-- -- /WARNING:/ This operation can be very inefficient. If possible, use
-- -- 'enumFromStepN' instead.
-- enumFromThenTo :: Enum a => a -> a -> a -> Vector a
-- {-# INLINE enumFromThenTo #-}
-- enumFromThenTo = G.enumFromThenTo

-- -- Concatenation
-- -- -------------

-- -- | /O(n)/ Prepend an element.
-- cons :: a -> Vector a -> Vector a
-- {-# INLINE cons #-}
-- cons = G.cons

-- -- | /O(n)/ Append an element.
-- snoc :: Vector a -> a -> Vector a
-- {-# INLINE snoc #-}
-- snoc = G.snoc

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors.
(++) :: Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
v ++ w = unfoldrExactN (length v + length w) (\i -> (if i < length v then v ! i else w ! i, i + 1)) 0

-- | /O(n)/ Concatenate all vectors in the list.
-- TODO: this could probably be done in a fusible way
concat :: [Vector a] -> Vector a
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
-- -- results in a vector.
-- replicateM :: Monad m => Int -> m a -> m (Vector a)
-- {-# INLINE replicateM #-}
-- replicateM n m = unfoldrExactNM n (\() -> do x <- m; Prelude.return (x, ())) ()
-- {-# SPECIALIZE replicateM :: Int -> Prelude.IO a -> Prelude.IO (Vector a) #-}

-- -- | /O(n)/ Construct a vector of the given length by applying the monadic
-- -- action to each index.
-- generateM :: Monad m => Int -> (Int -> m a) -> m (Vector a)
-- {-# INLINE generateM #-}
-- generateM n f = iunfoldrNM n (\i () -> do x <- f i; Prelude.return (Just (x, ()))) ()
-- {-# SPECIALIZE generateM :: Int -> (Int -> Prelude.IO a) -> Prelude.IO (Vector a) #-}

-- -- | /O(n)/ Apply the monadic function \(\max(n - 1, 0)\) times to an initial value, producing a vector
-- -- of length \(\max(n, 0)\). The 0th element will contain the initial value, which is why there
-- -- is one less function application than the number of elements in the produced vector.
-- --
-- -- For a non-monadic version, see `iterateN`.
-- --
-- -- @since 0.12.0.0
-- iterateNM :: Monad m => Int -> (a -> m a) -> a -> m (Vector a)
-- {-# INLINE iterateNM #-}
-- -- TODO: this doesn't produce optimal Core:
-- iterateNM n f x0 = unfoldrNM n (\ !x -> do x' <- f x; Prelude.return (x' `Prelude.seq` Just (x, x'))) x0
-- -- TODO: all monadic functions should have specialize for IO:
-- {-# SPECIALIZE iterateNM :: Int -> (a -> Prelude.IO a) -> a -> Prelude.IO (Vector a) #-}
-- -- TODO: consider if we really want this:
-- {-# SPECIALIZE iterateNM :: Int -> (a -> Identity a) -> a -> Identity (Vector a) #-}

-- -- -- | Execute the monadic action and freeze the resulting vector.
-- -- --
-- -- -- @
-- -- -- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- -- -- @
-- -- create :: (forall s. ST s (MVector s a)) -> Vector a
-- -- {-# INLINE create #-}
-- -- -- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
-- -- create p = G.create p

-- -- -- | Execute the monadic action and freeze the resulting vectors.
-- -- createT :: Traversable.Traversable f => (forall s. ST s (f (MVector s a))) -> f (Vector a)
-- -- {-# INLINE createT #-}
-- -- createT p = G.createT p



-- -- -- Restricting memory usage
-- -- -- ------------------------

-- -- -- | /O(n)/ Yield the argument, but force it not to retain any extra memory,
-- -- -- by copying it.
-- -- --
-- -- -- This is especially useful when dealing with slices. For example:
-- -- --
-- -- -- > force (slice 0 2 <huge vector>)
-- -- --
-- -- -- Here, the slice retains a reference to the huge vector. Forcing it creates
-- -- -- a copy of just the elements that belong to the slice and allows the huge
-- -- -- vector to be garbage collected.
-- -- force :: Vector a -> Vector a
-- -- {-# INLINE force #-}
-- -- force = G.force

-- -- Bulk updates
-- -- ------------

-- -- | /O(m+n)/ For each pair @(i,a)@ from the list of index/value pairs,
-- -- replace the vector element at position @i@ by @a@.
-- --
-- -- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
-- --
-- (//) :: Vector a   -- ^ initial vector (of length @m@)
--                 -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
--                 -> Vector a
-- {-# INLINE (//) #-}
-- (//) = (G.//)

-- -- | /O(m+n)/ For each pair @(i,a)@ from the vector of index/value pairs,
-- -- replace the vector element at position @i@ by @a@.
-- --
-- -- > update <5,9,2,7> <(2,1),(0,3),(2,8)> = <3,9,8,7>
-- --
-- update :: Vector a        -- ^ initial vector (of length @m@)
--        -> Vector (Int, a) -- ^ vector of index/value pairs (of length @n@)
--        -> Vector a
-- {-# INLINE update #-}
-- update = G.update

-- -- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- -- corresponding value @a@ from the value vector, replace the element of the
-- -- initial vector at position @i@ by @a@.
-- --
-- -- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
-- --
-- -- The function 'update' provides the same functionality and is usually more
-- -- convenient.
-- --
-- -- @
-- -- update_ xs is ys = 'update' xs ('zip' is ys)
-- -- @
-- update_ :: Vector a   -- ^ initial vector (of length @m@)
--         -> Vector Int -- ^ index vector (of length @n1@)
--         -> Vector a   -- ^ value vector (of length @n2@)
--         -> Vector a
-- {-# INLINE update_ #-}
-- update_ = G.update_

-- -- | Same as ('//'), but without bounds checking.
-- unsafeUpd :: Vector a -> [(Int, a)] -> Vector a
-- {-# INLINE unsafeUpd #-}
-- unsafeUpd = G.unsafeUpd

-- -- | Same as 'update', but without bounds checking.
-- unsafeUpdate :: Vector a -> Vector (Int, a) -> Vector a
-- {-# INLINE unsafeUpdate #-}
-- unsafeUpdate = G.unsafeUpdate

-- -- | Same as 'update_', but without bounds checking.
-- unsafeUpdate_ :: Vector a -> Vector Int -> Vector a -> Vector a
-- {-# INLINE unsafeUpdate_ #-}
-- unsafeUpdate_ = G.unsafeUpdate_

-- -- Accumulations
-- -- -------------

-- -- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- -- @a@ at position @i@ by @f a b@.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.accum (+) (V.fromList [1000,2000,3000]) [(2,4),(1,6),(0,3),(1,10)]
-- -- [1003,2016,3004]
-- accum :: (a -> b -> a) -- ^ accumulating function @f@
--       -> Vector a      -- ^ initial vector (of length @m@)
--       -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
--       -> Vector a
-- {-# INLINE accum #-}
-- accum = G.accum

-- -- | /O(m+n)/ For each pair @(i,b)@ from the vector of pairs, replace the vector
-- -- element @a@ at position @i@ by @f a b@.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.accumulate (+) (V.fromList [1000,2000,3000]) (V.fromList [(2,4),(1,6),(0,3),(1,10)])
-- -- [1003,2016,3004]
-- accumulate :: (a -> b -> a)  -- ^ accumulating function @f@
--             -> Vector a       -- ^ initial vector (of length @m@)
--             -> Vector (Int,b) -- ^ vector of index/value pairs (of length @n@)
--             -> Vector a
-- {-# INLINE accumulate #-}
-- accumulate = G.accumulate

-- -- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- -- corresponding value @b@ from the value vector,
-- -- replace the element of the initial vector at
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
--             -> Vector a      -- ^ initial vector (of length @m@)
--             -> Vector Int    -- ^ index vector (of length @n1@)
--             -> Vector b      -- ^ value vector (of length @n2@)
--             -> Vector a
-- {-# INLINE accumulate_ #-}
-- accumulate_ = G.accumulate_

-- -- | Same as 'accum', but without bounds checking.
-- unsafeAccum :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
-- {-# INLINE unsafeAccum #-}
-- unsafeAccum = G.unsafeAccum

-- -- | Same as 'accumulate', but without bounds checking.
-- unsafeAccumulate :: (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
-- {-# INLINE unsafeAccumulate #-}
-- unsafeAccumulate = G.unsafeAccumulate

-- -- | Same as 'accumulate_', but without bounds checking.
-- unsafeAccumulate_
--   :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
-- {-# INLINE unsafeAccumulate_ #-}
-- unsafeAccumulate_ = G.unsafeAccumulate_

-- -- Permutations
-- -- ------------

-- | /O(n)/ Reverse a vector.
reverse :: Vector a -> Vector a
{-# INLINE reverse #-}
reverse v = generate (length v) (\i -> unsafeIndex v (length v - 1 - i))

-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@, but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute vx vi = generate (length vi) (\i -> vx ! unsafeIndex vi i)

-- | Same as 'backpermute', but without bounds checking.
unsafeBackpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute vx vi = generate (length vi) (\i -> unsafeIndex vx (unsafeIndex vi i))

-- -- Safe destructive updates
-- -- ------------------------

-- -- | Apply a destructive operation to a vector. The operation may be
-- -- performed in place if it is safe to do so and will modify a copy of the
-- -- vector otherwise (see 'Data.Vector.Generic.New.New' for details).
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> import qualified Data.Vector.Mutable as MV
-- -- >>> V.modify (\v -> MV.write v 0 'x') $ V.replicate 4 'a'
-- -- "xaaa"
-- modify :: (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
-- {-# INLINE modify #-}
-- modify p = G.modify p

-- -- Indexing
-- -- --------

-- TODO: this is probably not worth it without fusion:
--
-- -- | /O(n)/ Pair each element in a vector with its index.
-- indexed :: Vector a -> Vector (Int,a)
-- {-# INLINE indexed #-}
-- indexed = G.indexed

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector.
-- Warning: does not fuse, this will allocate a new copy of the vector.
-- Consider using explicit streaming (TODO) if you compose this with other combinators.
map :: (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map f v = iunfoldrExactN (length v) (\i () -> let !x = unsafeIndex v i in (f x, ())) ()

-- | /O(n)/ Apply a function to every element of a vector and its index.
imap :: (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap f v = iunfoldrExactN (length v) (\i () -> let !x = unsafeIndex v i in (f i x, ())) ()

-- -- | Map a function over a vector and concatenate the results.
-- concatMap :: (a -> Vector b) -> Vector a -> Vector b
-- {-# INLINE concatMap #-}
-- concatMap = G.concatMap

-- -- | Map a function to every element of a vector and its index, and concatenate the results.
-- --
-- -- @since 0.13.3.0
-- iconcatMap :: (Int -> a -> Vector b) -> Vector a -> Vector b
-- {-# INLINE iconcatMap #-}
-- iconcatMap = G.iconcatMap

-- -- Monadic mapping
-- -- ---------------

-- -- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- -- vector of results.
-- mapM :: Monad m => (a -> m b) -> Vector a -> m (Vector b)
-- {-# INLINE mapM #-}
-- mapM f v = _
  
--  -- iunfoldrNM (length v) (\i () -> let !x = unsafeIndex v i in do y <- f x; Prelude.return (Just (y, ()))) ()
-- {-# SPECIALIZE mapM :: (a -> Prelude.IO b) -> Vector a -> Prelude.IO (Vector b) #-}

-- -- | /O(n)/ Apply the monadic action to every element of a vector and its
-- -- index, yielding a vector of results.
-- imapM :: Monad m => (Int -> a -> m b) -> Vector a -> m (Vector b)
-- {-# INLINE imapM #-}
-- imapM f v = iunfoldrNM (length v) (\i () -> let !x = unsafeIndex v i in do y <- f i x; Prelude.return (Just (y, ()))) ()
-- {-# SPECIALIZE imapM :: (Int -> a -> Prelude.IO b) -> Vector a -> Prelude.IO (Vector b) #-}

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results.
mapM_ :: Monad m => (a -> m b) -> Vector a -> m ()
{-# INLINE mapM_ #-}
mapM_ f v = foldr (\ !x xs -> f x Prelude.>> xs) (Prelude.return ()) v
{-# SPECIALIZE mapM_ :: (a -> Prelude.IO b) -> Vector a -> Prelude.IO () #-}

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, ignoring the results.
imapM_ :: Monad m => (Int -> a -> m b) -> Vector a -> m ()
{-# INLINE imapM_ #-}
imapM_ f v = ifoldr (\i x xs -> x `Prelude.seq` (f i x Prelude.>> xs)) (Prelude.return ()) v
{-# SPECIALIZE imapM_ :: (Int -> a -> Prelude.IO b) -> Vector a -> Prelude.IO () #-}

-- -- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- -- vector of results. Equivalent to @flip 'mapM'@.
-- forM :: Monad m => Vector a -> (a -> m b) -> m (Vector b)
-- {-# INLINE forM #-}
-- forM v f = generateM (length v) (\i -> let !x = unsafeIndex v i in f x)
-- {-# SPECIALIZE forM :: Vector a -> (a -> Prelude.IO b) -> Prelude.IO (Vector b) #-}

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: Monad m => Vector a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ v f = foldr (\x m -> f x Prelude.>> m) (Prelude.return ()) v
{-# SPECIALIZE forM_ :: Vector a -> (a -> Prelude.IO b) -> Prelude.IO () #-}

-- -- | /O(n)/ Apply the monadic action to all elements of the vector and their indices, yielding a
-- -- vector of results. Equivalent to @'flip' 'imapM'@.
-- --
-- -- @since 0.12.2.0
-- iforM :: Monad m => Vector a -> (Int -> a -> m b) -> m (Vector b)
-- {-# INLINE iforM #-}
-- iforM v f = generateM (length v) (\i -> let !x = unsafeIndex v i in f i x)
-- {-# SPECIALIZE iforM :: Vector a -> (Int -> a -> Prelude.IO b) -> Prelude.IO (Vector b) #-}

-- | /O(n)/ Apply the monadic action to all elements of the vector and their indices
-- and ignore the results. Equivalent to @'flip' 'imapM_'@.
--
-- @since 0.12.2.0
iforM_ :: Monad m => Vector a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ v f = ifoldr (\i x m -> f i x Prelude.>> m) (Prelude.return ()) v
{-# SPECIALIZE iforM_ :: Vector a -> (Int -> a -> Prelude.IO b) -> Prelude.IO () #-}

-- -- Zipping
-- -- -------

-- -- | /O(min(m,n))/ Zip two vectors with the given function.
-- zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
-- {-# INLINE zipWith #-}
-- zipWith = G.zipWith

-- -- | Zip three vectors with the given function.
-- zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
-- {-# INLINE zipWith3 #-}
-- zipWith3 = G.zipWith3

-- zipWith4 :: (a -> b -> c -> d -> e)
--          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
-- {-# INLINE zipWith4 #-}
-- zipWith4 = G.zipWith4

-- zipWith5 :: (a -> b -> c -> d -> e -> f)
--          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
--          -> Vector f
-- {-# INLINE zipWith5 #-}
-- zipWith5 = G.zipWith5

-- zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
--          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
--          -> Vector f -> Vector g
-- {-# INLINE zipWith6 #-}
-- zipWith6 = G.zipWith6

-- -- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- -- elements' indices.
-- izipWith :: (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
-- {-# INLINE izipWith #-}
-- izipWith = G.izipWith

-- -- | Zip three vectors and their indices with the given function.
-- izipWith3 :: (Int -> a -> b -> c -> d)
--           -> Vector a -> Vector b -> Vector c -> Vector d
-- {-# INLINE izipWith3 #-}
-- izipWith3 = G.izipWith3

-- izipWith4 :: (Int -> a -> b -> c -> d -> e)
--           -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
-- {-# INLINE izipWith4 #-}
-- izipWith4 = G.izipWith4

-- izipWith5 :: (Int -> a -> b -> c -> d -> e -> f)
--           -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
--           -> Vector f
-- {-# INLINE izipWith5 #-}
-- izipWith5 = G.izipWith5

-- izipWith6 :: (Int -> a -> b -> c -> d -> e -> f -> g)
--           -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
--           -> Vector f -> Vector g
-- {-# INLINE izipWith6 #-}
-- izipWith6 = G.izipWith6

-- -- | /O(min(m,n))/ Zip two vectors.
-- zip :: Vector a -> Vector b -> Vector (a, b)
-- {-# INLINE zip #-}
-- zip = G.zip

-- -- | Zip together three vectors into a vector of triples.
-- zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
-- {-# INLINE zip3 #-}
-- zip3 = G.zip3

-- zip4 :: Vector a -> Vector b -> Vector c -> Vector d
--      -> Vector (a, b, c, d)
-- {-# INLINE zip4 #-}
-- zip4 = G.zip4

-- zip5 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e
--      -> Vector (a, b, c, d, e)
-- {-# INLINE zip5 #-}
-- zip5 = G.zip5

-- zip6 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
--      -> Vector (a, b, c, d, e, f)
-- {-# INLINE zip6 #-}
-- zip6 = G.zip6

-- -- Unzipping
-- -- ---------

-- -- | /O(min(m,n))/ Unzip a vector of pairs.
-- unzip :: Vector (a, b) -> (Vector a, Vector b)
-- {-# INLINE unzip #-}
-- unzip = G.unzip

-- unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
-- {-# INLINE unzip3 #-}
-- unzip3 = G.unzip3

-- unzip4 :: Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
-- {-# INLINE unzip4 #-}
-- unzip4 = G.unzip4

-- unzip5 :: Vector (a, b, c, d, e)
--        -> (Vector a, Vector b, Vector c, Vector d, Vector e)
-- {-# INLINE unzip5 #-}
-- unzip5 = G.unzip5

-- unzip6 :: Vector (a, b, c, d, e, f)
--        -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
-- {-# INLINE unzip6 #-}
-- unzip6 = G.unzip6

-- -- Monadic zipping
-- -- ---------------

-- -- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- -- vector of results.
-- zipWithM :: Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
-- {-# INLINE zipWithM #-}
-- zipWithM = G.zipWithM

-- -- | /O(min(m,n))/ Zip the two vectors with a monadic action that also takes
-- -- the element index and yield a vector of results.
-- izipWithM :: Monad m => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
-- {-# INLINE izipWithM #-}
-- izipWithM = G.izipWithM

-- -- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- -- results.
-- zipWithM_ :: Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m ()
-- {-# INLINE zipWithM_ #-}
-- zipWithM_ = G.zipWithM_

-- -- | /O(min(m,n))/ Zip the two vectors with a monadic action that also takes
-- -- the element index and ignore the results.
-- izipWithM_ :: Monad m => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m ()
-- {-# INLINE izipWithM_ #-}
-- izipWithM_ = G.izipWithM_

-- -- Filtering
-- -- ---------

-- -- | /O(n)/ Drop all elements that do not satisfy the predicate.
-- filter :: (a -> Bool) -> Vector a -> Vector a
-- {-# INLINE filter #-}
-- filter = G.filter

-- -- | /O(n)/ Drop all elements that do not satisfy the predicate which is applied to
-- -- the values and their indices.
-- ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
-- {-# INLINE ifilter #-}
-- ifilter = G.ifilter

-- -- | /O(n)/ Drop repeated adjacent elements. The first element in each group is returned.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.uniq $ V.fromList [1,3,3,200,3]
-- -- [1,3,200,3]
-- -- >>> import Data.Semigroup
-- -- >>> V.uniq $ V.fromList [ Arg 1 'a', Arg 1 'b', Arg 1 'c']
-- -- [Arg 1 'a']
-- uniq :: (Eq a) => Vector a -> Vector a
-- {-# INLINE uniq #-}
-- uniq = G.uniq

-- -- | /O(n)/ Map the values and collect the 'Just' results.
-- mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
-- {-# INLINE mapMaybe #-}
-- mapMaybe = G.mapMaybe

-- -- | /O(n)/ Map the indices/values and collect the 'Just' results.
-- imapMaybe :: (Int -> a -> Maybe b) -> Vector a -> Vector b
-- {-# INLINE imapMaybe #-}
-- imapMaybe = G.imapMaybe

-- -- | /O(n)/ Return a Vector of all the 'Just' values.
-- --
-- -- @since 0.12.2.0
-- catMaybes :: Vector (Maybe a) -> Vector a
-- {-# INLINE catMaybes #-}
-- catMaybes = mapMaybe id

-- -- | /O(n)/ Drop all elements that do not satisfy the monadic predicate.
-- filterM :: Monad m => (a -> m Bool) -> Vector a -> m (Vector a)
-- {-# INLINE filterM #-}
-- filterM = G.filterM

-- -- | /O(n)/ Apply the monadic function to each element of the vector and
-- -- discard elements returning 'Nothing'.
-- --
-- -- @since 0.12.2.0
-- mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
-- {-# INLINE mapMaybeM #-}
-- mapMaybeM = G.mapMaybeM

-- -- | /O(n)/ Apply the monadic function to each element of the vector and its index.
-- -- Discard elements returning 'Nothing'.
-- --
-- -- @since 0.12.2.0
-- imapMaybeM :: Monad m => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
-- {-# INLINE imapMaybeM #-}
-- imapMaybeM = G.imapMaybeM

-- -- | /O(n)/ Yield the longest prefix of elements satisfying the predicate.
-- -- The current implementation is not copy-free, unless the result vector is
-- -- fused away.
-- takeWhile :: (a -> Bool) -> Vector a -> Vector a
-- {-# INLINE takeWhile #-}
-- takeWhile = G.takeWhile

-- -- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- -- without copying.
-- dropWhile :: (a -> Bool) -> Vector a -> Vector a
-- {-# INLINE dropWhile #-}
-- dropWhile = G.dropWhile

-- -- Parititioning
-- -- -------------

-- -- | /O(n)/ Split the vector in two parts, the first one containing those
-- -- elements that satisfy the predicate and the second one those that don't. The
-- -- relative order of the elements is preserved at the cost of a sometimes
-- -- reduced performance compared to 'unstablePartition'.
-- partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
-- {-# INLINE partition #-}
-- partition = G.partition

-- -- | /O(n)/ Split the vector into two parts, the first one containing the
-- -- @`Left`@ elements and the second containing the @`Right`@ elements.
-- -- The relative order of the elements is preserved.
-- --
-- -- @since 0.12.1.0
-- partitionWith :: (a -> Either b c) -> Vector a -> (Vector b, Vector c)
-- {-# INLINE partitionWith #-}
-- partitionWith = G.partitionWith

-- -- | /O(n)/ Split the vector in two parts, the first one containing those
-- -- elements that satisfy the predicate and the second one those that don't.
-- -- The order of the elements is not preserved, but the operation is often
-- -- faster than 'partition'.
-- unstablePartition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
-- {-# INLINE unstablePartition #-}
-- unstablePartition = G.unstablePartition

-- -- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- -- the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.span (<4) $ V.generate 10 id
-- -- ([0,1,2,3],[4,5,6,7,8,9])
-- span :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
-- {-# INLINE span #-}
-- span = G.span

-- -- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- -- satisfy the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.break (>4) $ V.generate 10 id
-- -- ([0,1,2,3,4],[5,6,7,8,9])
-- break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
-- {-# INLINE break #-}
-- break = G.break

-- -- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- -- the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.spanR (>4) $ V.generate 10 id
-- -- ([5,6,7,8,9],[0,1,2,3,4])
-- --
-- -- @since 0.13.2.0
-- spanR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
-- {-# INLINE spanR #-}
-- spanR = G.spanR

-- -- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- -- satisfy the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.breakR (<5) $ V.generate 10 id
-- -- ([5,6,7,8,9],[0,1,2,3,4])
-- --
-- -- @since 0.13.2.0
-- breakR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
-- {-# INLINE breakR #-}
-- breakR = G.breakR

-- -- | /O(n)/ Split a vector into a list of slices, using a predicate function.
-- --
-- -- The concatenation of this list of slices is equal to the argument vector,
-- -- and each slice contains only equal elements, as determined by the equality
-- -- predicate function.
-- --
-- -- Does not fuse.
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> import           Data.Char (isUpper)
-- -- >>> V.groupBy (\a b -> isUpper a == isUpper b) (V.fromList "Mississippi River")
-- -- ["M","ississippi ","R","iver"]
-- --
-- -- See also 'Data.List.groupBy', 'group'.
-- --
-- -- @since 0.13.0.0
-- groupBy :: (a -> a -> Bool) -> Vector a -> [Vector a]
-- {-# INLINE groupBy #-}
-- groupBy = G.groupBy

-- -- | /O(n)/ Split a vector into a list of slices of the input vector.
-- --
-- -- The concatenation of this list of slices is equal to the argument vector,
-- -- and each slice contains only equal elements.
-- --
-- -- Does not fuse.
-- --
-- -- This is the equivalent of 'groupBy (==)'.
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.group (V.fromList "Mississippi")
-- -- ["M","i","ss","i","ss","i","pp","i"]
-- --
-- -- See also 'Data.List.group'.
-- --
-- -- @since 0.13.0.0
-- group :: Eq a => Vector a -> [Vector a]
-- {-# INLINE group #-}
-- group = G.groupBy (==)

-- -- Searching
-- -- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element.
elem :: Eq a => a -> Vector a -> Bool
{-# INLINE elem #-}
elem z = foldr (\x xs -> x == z Prelude.|| xs) Prelude.False

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem').
notElem :: Eq a => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem z v = Prelude.not (elem z v)

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find f = foldr (\x xs -> if f x then Just x else xs) Nothing

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex f = ifoldr (\i x xs -> if f x then Just i else xs) Nothing

-- -- | /O(n)/ Yield 'Just' the index of the /last/ element matching the predicate
-- -- or 'Nothing' if no such element exists.
-- --
-- -- Does not fuse.
-- findIndexR :: (a -> Bool) -> Vector a -> Maybe Int
-- {-# INLINE findIndexR #-}
-- findIndexR = G.findIndexR

-- -- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- -- order.
-- findIndices :: (a -> Bool) -> Vector a -> Vector Int
-- {-# INLINE findIndices #-}
-- findIndices = G.findIndices

-- | /O(n)/ Yield 'Just' the index of the first occurrence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: Eq a => a -> Vector a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex x = findIndex (== x)

-- -- | /O(n)/ Yield the indices of all occurrences of the given element in
-- -- ascending order. This is a specialised version of 'findIndices'.
-- elemIndices :: Eq a => a -> Vector a -> Vector Int
-- {-# INLINE elemIndices #-}
-- elemIndices = G.elemIndices

-- -- Folding
-- -- -------

-- | /O(n)/ Left fold.
foldl :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl k z v = go z 0 where
  go s i
    | i < length v = let !x = unsafeIndex v i in go (k s x) (i + 1)
    | otherwise = s

-- | /O(n)/ Left fold on non-empty vectors.
foldl1 :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 k v 
  | null v = error "foldr1 applied to empty vector"
  | otherwise = go (unsafeIndex v 0) 1 where
  go s i
    | i < length v - 1 = let !x = unsafeIndex v i in go (k s x) (i + 1)
    | otherwise = unsafeIndex v (length v - 1)

-- | /O(n)/ Left fold with strict accumulator.
foldl' :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' k z = \v -> 
  let
    go !s i
      | i < length v = let !x = unsafeIndex v i in go (k s x) (i + 1)
      | otherwise = s
  in go z 0

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator.
foldl1' :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' k v 
  | null v = error "foldr1 applied to empty vector"
  | otherwise = go (unsafeIndex v 0) 1 where
  go !s i
    | i < length v - 1 = let !x = unsafeIndex v i in go (k s x) (i + 1)
    | otherwise = unsafeIndex v (length v - 1)

-- | /O(n)/ Right fold.
foldr :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr k z = \v -> 
  let
    go i
      | i < length v = let !x = unsafeIndex v i in k x (go (i + 1))
      | otherwise = z
  in go 0

-- TODO: implement as left-to-right pass?
-- | /O(n)/ Right fold on non-empty vectors.
foldr1 :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 k v 
  | null v = error "foldr1 applied to empty vector"
  | otherwise = go (unsafeIndex v (length v - 1)) (length v - 2) where
  go s i
    | 0 <= i = let !x = unsafeIndex v i in go (k x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Right fold with a strict accumulator.
foldr' :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr' #-}
foldr' k z v = go z (length v - 1) where
  go !s i
    | 0 <= i = let !x = unsafeIndex v i in go (k x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator.
foldr1' :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1' #-}
foldr1' k v 
  | null v = error "foldr1 applied to empty vector"
  | otherwise = go (unsafeIndex v (length v - 1)) (length v - 2) where
  go !s i
    | 0 <= i = let !x = unsafeIndex v i in go (k x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Left fold using a function applied to each element and its index.
ifoldl :: (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl #-}
ifoldl k z v = go z 0 where
  go s i
    | i < length v = let !x = unsafeIndex v i in go (k s i x) (i + 1)
    | otherwise = s

-- | /O(n)/ Left fold with strict accumulator using a function applied to each element
-- and its index.
ifoldl' :: (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl' #-}
ifoldl' k z v = go z 0 where
  go !s i
    | i < length v = let !x = unsafeIndex v i in go (k s i x) (i + 1)
    | otherwise = s

-- | /O(n)/ Right fold using a function applied to each element and its index.
ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr #-}
ifoldr k z v = go 0 where
  go i
    | i < length v = let !x = unsafeIndex v i in k i x (go (i - 1))
    | otherwise = z

-- | /O(n)/ Right fold with strict accumulator using a function applied to each
-- element and its index.
ifoldr' :: (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr' #-}
ifoldr' k z v = go z (length v - 1) where
  go !s i
    | 0 <= i = let !x = unsafeIndex v i in go (k i x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Map each element of the structure to a monoid and combine
-- the results. It uses the same implementation as the corresponding method
-- of the 'Foldable' type class.
--
-- @since 0.12.2.0
foldMap :: (Monoid m) => (a -> m) -> Vector a -> m
{-# INLINE foldMap #-}
foldMap f = foldr (\x m -> f x Prelude.<> m) Prelude.mempty

-- | /O(n)/ Like 'foldMap', but strict in the accumulator. It uses the same
-- implementation as the corresponding method of the 'Foldable' type class.
-- Note that it's implemented in terms of 'foldl'', so it fuses in most
-- contGHC.
--
-- @since 0.12.2.0
foldMap' :: (Monoid m) => (a -> m) -> Vector a -> m
{-# INLINE foldMap' #-}
foldMap' f = foldr' (\x m -> f x Prelude.<> m) Prelude.mempty


-- -- Specialised folds
-- -- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.all even $ V.fromList [2, 4, 12]
-- True
-- >>> V.all even $ V.fromList [2, 4, 13]
-- False
-- >>> V.all even (V.empty :: V.Vector Int)
-- True
all :: (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all f v = foldr (\x xs -> f x Prelude.&& xs) Prelude.True v

-- | /O(n)/ Check if any element satisfies the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.any even $ V.fromList [1, 3, 7]
-- False
-- >>> V.any even $ V.fromList [3, 2, 13]
-- True
-- >>> V.any even (V.empty :: V.Vector Int)
-- False
any :: (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any f = foldr (\x xs -> f x Prelude.|| xs) Prelude.False

-- | /O(n)/ Check if all elements are 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.and $ V.fromList [True, False]
-- False
-- >>> V.and V.empty
-- True
and :: Vector Bool -> Bool
{-# INLINE and #-}
and = foldr (Prelude.&&) Prelude.True

-- | /O(n)/ Check if any element is 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.or $ V.fromList [True, False]
-- True
-- >>> V.or V.empty
-- False
or :: Vector Bool -> Bool
{-# INLINE or #-}
or = foldr (Prelude.||) Prelude.False

-- | /O(n)/ Compute the sum of the elements.
-- Warning: storing numbers (e.g. Int or Double) in a Vector
-- is inefficient because of redundant indirections.
-- Consider using unboxed vectors (TODO) instead.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.sum $ V.fromList [300,20,1]
-- 321
-- >>> V.sum (V.empty :: V.Vector Int)
-- 0
sum :: Num a => Vector a -> a
{-# INLINE sum #-}
sum = foldl' (+) 0

-- | /O(n)/ Compute the product of the elements.
-- Warning: storing numbers (e.g. Int or Double) in a Vector
-- is inefficient because of redundant indirections.
-- Consider using unboxed vectors (TODO) instead.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.product $ V.fromList [1,2,3,4]
-- 24
-- >>> V.product (V.empty :: V.Vector Int)
-- 1
product :: Num a => Vector a -> a
{-# INLINE product #-}
product = foldl' (*) 1

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.maximum $ V.fromList [2, 1]
-- 2
-- >>> import Data.Semigroup
-- >>> V.maximum $ V.fromList [Arg 1 'a', Arg 2 'b']
-- Arg 2 'b'
-- >>> V.maximum $ V.fromList [Arg 1 'a', Arg 1 'b']
-- Arg 1 'a'
maximum :: Ord a => Vector a -> a
{-# INLINE maximum #-}
maximum = foldl1' max

-- | /O(n)/ Yield the maximum element of the vector according to the
-- given comparison function. The vector may not be empty. In case of
-- a tie, the first occurrence wins. This behavior is different from
-- 'Data.List.maximumBy' which returns the last tie.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector as V
-- >>> V.maximumBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- (2,'a')
-- >>> V.maximumBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
maximumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy f = foldl1' (\x y -> case f x y of GT -> x; _ -> y)

-- | /O(n)/ Yield the maximum element of the vector by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The vector may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.maximumOn fst $ V.fromList [(2,'a'), (1,'b')]
-- (2,'a')
-- >>> V.maximumOn fst $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
--
-- @since 0.13.0.0
maximumOn :: Ord b => (a -> b) -> Vector a -> a
{-# INLINE maximumOn #-}
maximumOn f v = maybe (Prelude.error "maximumOn: empty vector") Prelude.fst (foldl' (\s x ->
  case s of
    Just (!y, !fy) ->
      let !fx = f x in if fx > fy then Just (x, fx) else Just (y, fy)
    Nothing -> let !fx = f x in Just (x, fx)
  ) Nothing v)
  

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.minimum $ V.fromList [2, 1]
-- 1
-- >>> import Data.Semigroup
-- >>> V.minimum $ V.fromList [Arg 2 'a', Arg 1 'b']
-- Arg 1 'b'
-- >>> V.minimum $ V.fromList [Arg 1 'a', Arg 1 'b']
-- Arg 1 'a'
minimum :: Ord a => Vector a -> a
{-# INLINE minimum #-}
minimum = foldl1' min

-- | /O(n)/ Yield the minimum element of the vector according to the
-- given comparison function. The vector may not be empty. In case of
-- a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector as V
-- >>> V.minimumBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- (1,'b')
-- >>> V.minimumBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
minimumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE minimumBy #-}
minimumBy f = foldl1' (\x y -> case f x y of LT -> x; _ -> y)

-- | /O(n)/ Yield the minimum element of the vector by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The vector may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.minimumOn fst $ V.fromList [(2,'a'), (1,'b')]
-- (1,'b')
-- >>> V.minimumOn fst $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
--
-- @since 0.13.0.0
minimumOn :: Ord b => (a -> b) -> Vector a -> a
{-# INLINE minimumOn #-}
minimumOn f v = maybe (Prelude.error "minimumOn: empty vector") Prelude.fst (foldl' (\s x ->
  case s of
    Just (!y, !fy) ->
      let !fx = f x in if fx < fy then Just (x, fx) else Just (y, fy)
    Nothing -> let !fx = f x in Just (x, fx)
  ) Nothing v)

-- -- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- -- may not be empty.
-- maxIndex :: Ord a => Vector a -> Int
-- {-# INLINE maxIndex #-}
-- maxIndex = G.maxIndex

-- -- | /O(n)/ Yield the index of the maximum element of the vector
-- -- according to the given comparison function. The vector may not be
-- -- empty. In case of a tie, the first occurrence wins.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import Data.Ord
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.maxIndexBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- -- 0
-- -- >>> V.maxIndexBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- -- 0
-- maxIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
-- {-# INLINE maxIndexBy #-}
-- maxIndexBy = G.maxIndexBy

-- -- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- -- may not be empty.
-- minIndex :: Ord a => Vector a -> Int
-- {-# INLINE minIndex #-}
-- minIndex = G.minIndex

-- -- | /O(n)/ Yield the index of the minimum element of the vector according to
-- -- the given comparison function. The vector may not be empty.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import Data.Ord
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.minIndexBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- -- 1
-- -- >>> V.minIndexBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- -- 0
-- minIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
-- {-# INLINE minIndexBy #-}
-- minIndexBy = G.minIndexBy

-- -- Monadic folds
-- -- -------------

-- | /O(n)/ Monadic fold.
foldM :: Monad m => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM #-}
-- TODO: this does not generate optimal Core/STG. i
-- I guess we'll need to implement these instead of the non-monadic folds.
foldM k z = foldl' (\m y -> do x <- m; k x y) (Prelude.return z)
{-# SPECIALIZE foldM :: (a -> b -> Prelude.IO a) -> a -> Vector b -> Prelude.IO a #-}

-- -- | /O(n)/ Monadic fold using a function applied to each element and its index.
-- ifoldM :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m a
-- {-# INLINE ifoldM #-}
-- ifoldM = G.ifoldM

-- -- | /O(n)/ Monadic fold over non-empty vectors.
-- fold1M :: Monad m => (a -> a -> m a) -> Vector a -> m a
-- {-# INLINE fold1M #-}
-- fold1M = G.fold1M

-- -- | /O(n)/ Monadic fold with strict accumulator.
-- foldM' :: Monad m => (a -> b -> m a) -> a -> Vector b -> m a
-- {-# INLINE foldM' #-}
-- foldM' = G.foldM'

-- -- | /O(n)/ Monadic fold with strict accumulator using a function applied to each
-- -- element and its index.
-- ifoldM' :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m a
-- {-# INLINE ifoldM' #-}
-- ifoldM' = G.ifoldM'

-- -- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator.
-- fold1M' :: Monad m => (a -> a -> m a) -> Vector a -> m a
-- {-# INLINE fold1M' #-}
-- fold1M' = G.fold1M'

-- -- | /O(n)/ Monadic fold that discards the result.
-- foldM_ :: Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
-- {-# INLINE foldM_ #-}
-- foldM_ = G.foldM_

-- -- | /O(n)/ Monadic fold that discards the result using a function applied to
-- -- each element and its index.
-- ifoldM_ :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
-- {-# INLINE ifoldM_ #-}
-- ifoldM_ = G.ifoldM_

-- -- | /O(n)/ Monadic fold over non-empty vectors that discards the result.
-- fold1M_ :: Monad m => (a -> a -> m a) -> Vector a -> m ()
-- {-# INLINE fold1M_ #-}
-- fold1M_ = G.fold1M_

-- -- | /O(n)/ Monadic fold with strict accumulator that discards the result.
-- foldM'_ :: Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
-- {-# INLINE foldM'_ #-}
-- foldM'_ = G.foldM'_

-- -- | /O(n)/ Monadic fold with strict accumulator that discards the result
-- -- using a function applied to each element and its index.
-- ifoldM'_ :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
-- {-# INLINE ifoldM'_ #-}
-- ifoldM'_ = G.ifoldM'_

-- -- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- -- that discards the result.
-- fold1M'_ :: Monad m => (a -> a -> m a) -> Vector a -> m ()
-- {-# INLINE fold1M'_ #-}
-- fold1M'_ = G.fold1M'_

-- -- Monadic sequencing
-- -- ------------------

-- -- -- | Evaluate each action and collect the results.
-- sequence :: Monad m => Vector (m a) -> m (Vector a)
-- {-# INLINE sequence #-}
-- sequence v = generateM (length v) (\i -> unsafeIndex v i)
-- {-# SPECIALIZE sequence :: Vector (Prelude.IO a) -> Prelude.IO (Vector a) #-}

-- -- | Evaluate each action and discard the results.
-- sequence_ :: Monad m => Vector (m a) -> m ()
-- {-# INLINE sequence_ #-}
-- sequence_ = foldr (\m xs -> m Prelude.>> xs) (Prelude.return ())
-- {-# SPECIALIZE sequence_ :: Vector (Prelude.IO a) -> Prelude.IO () #-}

-- Scans
-- -----

-- -- | /O(n)/ Left-to-right prescan.
-- --
-- -- @
-- -- prescanl f z = 'init' . 'scanl' f z
-- -- @
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.prescanl (+) 0 (V.fromList [1,2,3,4])
-- -- [0,1,3,6]
-- prescanl :: (a -> b -> a) -> a -> Vector b -> Vector a
-- {-# INLINE prescanl #-}
-- prescanl = G.prescanl

-- | /O(n)/ Left-to-right prescan with strict accumulator.
prescanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' k z v = iunfoldrExactN (length v) (\i s -> let !x = unsafeIndex v i; s' = k s x in (s, s')) z

-- -- | /O(n)/ Left-to-right postscan.
-- --
-- -- @
-- -- postscanl f z = 'tail' . 'scanl' f z
-- -- @
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.postscanl (+) 0 (V.fromList [1,2,3,4])
-- -- [1,3,6,10]
-- postscanl :: (a -> b -> a) -> a -> Vector b -> Vector a
-- {-# INLINE postscanl #-}
-- postscanl = G.postscanl

-- | /O(n)/ Left-to-right postscan with strict accumulator.
postscanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' k z v = iunfoldrExactN (length v) (\i s -> let !x = unsafeIndex v i; s' = k s x in (s', s')) z

-- -- | /O(n)/ Left-to-right scan.
-- --
-- -- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- -- >   where y1 = z
-- -- >         yi = f y(i-1) x(i-1)
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.scanl (+) 0 (V.fromList [1,2,3,4])
-- -- [0,1,3,6,10]
-- scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
-- {-# INLINE scanl #-}
-- scanl = G.scanl

-- | /O(n)/ Left-to-right scan with strict accumulator.
scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' k z v = iscanl' (\_ -> k) z v

-- -- | /O(n)/ Left-to-right scan over a vector with its index.
-- --
-- -- @since 0.12.0.0
-- iscanl :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
-- {-# INLINE iscanl #-}
-- iscanl = G.iscanl

-- | /O(n)/ Left-to-right scan over a vector (strictly) with its index.
--
-- @since 0.12.0.0
iscanl' :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE iscanl' #-}
iscanl' k z v = iunfoldrExactN (length v + 1) (\i s -> if i == length v then (s,s) else let !x = unsafeIndex v i; s' = k i s x in (s, s')) z

-- -- | /O(n)/ Initial-value free left-to-right scan over a vector.
-- --
-- -- > scanl f <x1,...,xn> = <y1,...,yn>
-- -- >   where y1 = x1
-- -- >         yi = f y(i-1) xi
-- --
-- -- Note: Since 0.13, application of this to an empty vector no longer
-- -- results in an error; instead it produces an empty vector.
-- --
-- -- ==== __Examples__
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.scanl1 min $ V.fromListN 5 [4,2,4,1,3]
-- -- [4,2,2,1,1]
-- -- >>> V.scanl1 max $ V.fromListN 5 [1,3,2,5,4]
-- -- [1,3,3,5,5]
-- -- >>> V.scanl1 min (V.empty :: V.Vector Int)
-- -- []
-- scanl1 :: (a -> a -> a) -> Vector a -> Vector a
-- {-# INLINE scanl1 #-}
-- scanl1 = G.scanl1

-- | /O(n)/ Initial-value free left-to-right scan over a vector with a strict accumulator.
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector as V
-- >>> V.scanl1' min $ V.fromListN 5 [4,2,4,1,3]
-- [4,2,2,1,1]
-- >>> V.scanl1' max $ V.fromListN 5 [1,3,2,5,4]
-- [1,3,3,5,5]
-- >>> V.scanl1' min (V.empty :: V.Vector Int)
-- []
scanl1' :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' k v = iunfoldrExactN (length v) (\i s ->
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
-- prescanr :: (a -> b -> b) -> b -> Vector a -> Vector b
-- {-# INLINE prescanr #-}
-- prescanr = G.prescanr

-- -- | /O(n)/ Right-to-left prescan with strict accumulator.
-- prescanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
-- {-# INLINE prescanr' #-}
-- prescanr' = G.prescanr'

-- -- | /O(n)/ Right-to-left postscan.
-- postscanr :: (a -> b -> b) -> b -> Vector a -> Vector b
-- {-# INLINE postscanr #-}
-- postscanr = G.postscanr

-- -- | /O(n)/ Right-to-left postscan with strict accumulator.
-- postscanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
-- {-# INLINE postscanr' #-}
-- postscanr' = G.postscanr'

-- -- | /O(n)/ Right-to-left scan.
-- scanr :: (a -> b -> b) -> b -> Vector a -> Vector b
-- {-# INLINE scanr #-}
-- scanr = G.scanr

-- -- | /O(n)/ Right-to-left scan with strict accumulator.
-- scanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
-- {-# INLINE scanr' #-}
-- scanr' = G.scanr'

-- -- | /O(n)/ Right-to-left scan over a vector with its index.
-- --
-- -- @since 0.12.0.0
-- iscanr :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
-- {-# INLINE iscanr #-}
-- iscanr = G.iscanr

-- -- | /O(n)/ Right-to-left scan over a vector (strictly) with its index.
-- --
-- -- @since 0.12.0.0
-- iscanr' :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
-- {-# INLINE iscanr' #-}
-- iscanr' = G.iscanr'

-- -- | /O(n)/ Right-to-left, initial-value free scan over a vector.
-- --
-- -- Note: Since 0.13, application of this to an empty vector no longer
-- -- results in an error; instead it produces an empty vector.
-- --
-- -- ==== __Examples__
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.scanr1 min $ V.fromListN 5 [3,1,4,2,4]
-- -- [1,1,2,2,4]
-- -- >>> V.scanr1 max $ V.fromListN 5 [4,5,2,3,1]
-- -- [5,5,3,3,1]
-- -- >>> V.scanr1 min (V.empty :: V.Vector Int)
-- -- []
-- scanr1 :: (a -> a -> a) -> Vector a -> Vector a
-- {-# INLINE scanr1 #-}
-- scanr1 = G.scanr1

-- -- | /O(n)/ Right-to-left, initial-value free scan over a vector with a strict
-- -- accumulator.
-- --
-- -- Note: Since 0.13, application of this to an empty vector no longer
-- -- results in an error; instead it produces an empty vector.
-- --
-- -- ==== __Examples__
-- -- >>> import qualified Data.Vector as V
-- -- >>> V.scanr1' min $ V.fromListN 5 [3,1,4,2,4]
-- -- [1,1,2,2,4]
-- -- >>> V.scanr1' max $ V.fromListN 5 [4,5,2,3,1]
-- -- [5,5,3,3,1]
-- -- >>> V.scanr1' min (V.empty :: V.Vector Int)
-- -- []
-- scanr1' :: (a -> a -> a) -> Vector a -> Vector a
-- {-# INLINE scanr1' #-}
-- scanr1' = G.scanr1'

-- -- Comparisons
-- -- ------------------------

-- | /O(n)/ Check if two vectors are equal using the supplied equality
-- predicate.
--
-- @since 0.12.2.0
eqBy :: (a -> b -> Bool) -> Vector a -> Vector b -> Bool
{-# INLINE eqBy #-}
eqBy eq v w = ifoldr (\i x xs -> let !y = unsafeIndex w i in eq x y Prelude.&& xs) Prelude.True v

-- | /O(n)/ Compare two vectors using the supplied comparison function for
-- vector elements. Comparison works the same as for lists (lexicographically).
--
-- > cmpBy compare == compare
--
-- @since 0.12.2.0
cmpBy :: (a -> b -> Ordering) -> Vector a -> Vector b -> Ordering
{-# INLINE cmpBy #-}
cmpBy cmp v w = ifoldr (\i x xs -> let !y = unsafeIndex w i in cmp x y Prelude.<> xs) Prelude.EQ v

-- -- Conversions - Lists
-- -- ------------------------

-- | /O(n)/ Convert a vector to a list. Can fuse!
toList :: Vector a -> [a]
{-# INLINE toList #-}
toList v = GHC.build (\c n ->
  let 
    go i
      | i < length v = let !x = unsafeIndex v i in x `c` go (i + 1)
      | otherwise = n
  in go 0)

-- -- | /O(n)/ Convert a list to a vector. During the operation, the 
-- -- vector’s capacity will be doubling until the list's contents are 
-- -- in the vector. Depending on the list’s size, up to half of the vector’s 
-- -- capacity might be empty. If you’d rather avoid this, you can use 
-- -- 'fromListN', which will provide the exact space the list requires but will 
-- -- prevent list fusion, or @'force' . 'fromList'@, which will create the 
-- -- vector and then copy it without the superfluous space.
-- --
-- -- @since 0.3
-- fromList :: [a] -> Vector a
-- {-# INLINE fromList #-}
-- fromList = G.fromList

-- | /O(n)/ Convert the first @n@ elements of a list to a vector. It's
-- expected that the supplied list will be exactly @n@ elements long. As
-- an optimization, this function allocates a buffer for @n@ elements, which
-- could be used for DoS-attacks by exhausting the memory if an attacker controls
-- that parameter.
--
-- can fuse!
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
-- fromListN :: Int -> [a] -> Vector a
-- {-# INLINE fromListN #-}
-- fromListN @a (GHC.I# n) xs = GHC.runRW# (\s0 ->
--   let
--     marr :: GHC.MutableArray# GHC.RealWorld (Strict a)
--     !(# s1, marr #) = GHC.newArray# n (GHC.unsafeCoerce# ()) s0
--     endNormal s = 
--       let !(# _, arr' #) = GHC.unsafeFreezeArray# marr s
--       in UnsafeVector arr'
--   in Prelude.foldr
--     (\x go -> GHC.oneShot (\(MkFromListNSt (# s, i #)) ->
--       case i GHC.<# n of
--         0# -> endNormal s
--         _ ->
--           let !s' = GHC.writeArray# marr i (Strict x) s
--           in go (MkFromListNSt (# s', i GHC.+# 1# #))))
--     (\(MkFromListNSt (# s, i #)) ->
--       case i GHC.==# n of
--         0# -> 
--           let !(# _, arr' #) = GHC.freezeArray# marr 0# i s
--           in UnsafeVector arr'
--         _ -> endNormal s)
--     xs 
--     (MkFromListNSt (# s1, 0# #)))

-- data FromListNSt = MkFromListNSt (# GHC.State# GHC.RealWorld, GHC.Int# #)

fromListN :: Int -> [a] -> Vector a
{-# INLINE fromListN #-}
fromListN n xs = runST (do
  m <- M.unsafeNew n
  Prelude.foldr 
    (\x go -> GHC.oneShot (\i -> 
      if i < n
        then do M.unsafeWrite m i x; go (i + 1)
        else unsafeFreeze m))
    (\i -> if i == n then unsafeFreeze m else freeze (M.unsafeTakeL i (M.whole m)))
    xs
    0)

-- -- Applicative
-- -- -----------

-- -- | Construct a vector of the given length by applying the applicative
-- -- action to each index.
-- --
-- -- @since NEXT_VERSION
-- generateA :: Applicative f => Int -> (Int -> f a) -> f (Vector a)
-- generateA n f = runST $ 

-- -- | Execute the applicative action the given number of times and store the
-- -- results in a vector.
-- --
-- -- @since NEXT_VERSION
-- replicateA :: (Applicative f) => Int -> f a -> f (Vector a)
-- {-# INLINE replicateA #-}
-- replicateA = G.replicateA

-- -- | Apply the applicative action to all elements of the vector, yielding a
-- -- vector of results.
-- --
-- -- @since NEXT_VERSION
-- traverse :: (Applicative f)
--          => (a -> f b) -> Vector a -> f (Vector b)
-- {-# INLINE traverse #-}
-- traverse = G.traverse

-- -- | Apply the applicative action to every element of a vector and its
-- -- index, yielding a vector of results.
-- --
-- -- @since NEXT_VERSION
-- itraverse :: (Applicative f)
--           => (Int -> a -> f b) -> Vector a -> f (Vector b)
-- {-# INLINE itraverse #-}
-- itraverse = G.itraverse

-- -- | Apply the applicative action to all elements of the vector, yielding a
-- -- vector of results. This is flipped version of 'traverse'.
-- --
-- -- @since NEXT_VERSION
-- forA :: (Applicative f)
--      => Vector a -> (a -> f b) -> f (Vector b)
-- {-# INLINE forA #-}
-- forA = G.forA

-- -- | Apply the applicative action to every element of a vector and its
-- --   index, yielding a vector of results. This is flipped version of 'itraverse'.
-- --
-- -- @since NEXT_VERSION
-- iforA :: (Applicative f)
--       => Vector a -> (Int -> a -> f b) -> f (Vector b)
-- {-# INLINE iforA #-}
-- iforA = G.iforA

-- -- | Map each element of a structure to an 'Applicative' action, evaluate these
-- --   actions from left to right, and ignore the results.
-- --
-- -- @since NEXT_VERSION
-- traverse_ :: (Applicative f)
--           => (a -> f b) -> Vector a -> f ()
-- {-# INLINE traverse_ #-}
-- traverse_ = G.traverse_

-- -- | Map each element of a structure to an 'Applicative' action, evaluate these
-- --   actions from left to right, and ignore the results.
-- --
-- -- @since NEXT_VERSION
-- itraverse_ :: (Applicative f)
--            => (Int -> a -> f b) -> Vector a -> f ()
-- {-# INLINE itraverse_ #-}
-- itraverse_ = G.itraverse_

-- -- | Map each element of a structure to an 'Applicative' action, evaluate these
-- --   actions from left to right, and ignore the results.
-- --
-- -- @since NEXT_VERSION
-- forA_ :: (Applicative f)
--       => Vector a -> (a -> f b) -> f ()
-- {-# INLINE forA_ #-}
-- forA_ = G.forA_

-- -- | Map each element of a structure to an 'Applicative' action, evaluate these
-- --   actions from left to right, and ignore the results.
-- --
-- -- @since NEXT_VERSION
-- iforA_ :: (Applicative f)
--       => Vector a -> (Int -> a -> f b) -> f ()
-- {-# INLINE iforA_ #-}
-- iforA_ = G.iforA_


-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafely convert a mutable vector to an immutable one without
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze :: M.STVector s a -> ST s (Vector a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze (M.UnsafeSTVector marr) = GHC.ST (\s ->
  case GHC.unsafeFreezeSmallArray# marr s of
    (# s', arr #) -> (# s', UnsafeVector arr #))

data VectorSlice a = UnsafeVectorSlice {-# UNPACK #-} !(Vector a) !Int !Int

whole :: Vector a -> VectorSlice a
whole v = UnsafeVectorSlice v 0 (length v)

unsafeTakeL :: Int -> VectorSlice a -> VectorSlice a
unsafeTakeL n (UnsafeVectorSlice m off _) = UnsafeVectorSlice m off n

unsafeTakeR :: Int -> VectorSlice a -> VectorSlice a
unsafeTakeR n (UnsafeVectorSlice m off len) = UnsafeVectorSlice m (off + len - n) n

unsafeDropL :: Int -> VectorSlice a -> VectorSlice a
unsafeDropL n (UnsafeVectorSlice m off len) = UnsafeVectorSlice m (off + n) (len - n)

unsafeDropR :: Int -> VectorSlice a -> VectorSlice a
unsafeDropR n (UnsafeVectorSlice m off len) = UnsafeVectorSlice m off (len - n)

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: M.STVectorSlice s a -> ST s (Vector a)
{-# INLINE freeze #-}
freeze (M.UnsafeSTVectorSlice (M.UnsafeSTVector marr) (GHC.I# off) (GHC.I# len)) = GHC.ST (\s ->
  case GHC.freezeSmallArray# marr off len s of
    (# s', arr #) -> (# s', UnsafeVector arr #))

-- -- | /O(1)/ Unsafely convert an immutable vector to a mutable one
-- -- without copying. Note that this is a very dangerous function and
-- -- generally it's only safe to read from the resulting vector. In this
-- -- case, the immutable vector could be used safely as well.
-- --
-- -- Problems with mutation happen because GHC has a lot of freedom to
-- -- introduce sharing. As a result mutable vectors produced by
-- -- @unsafeThaw@ may or may not share the same underlying buffer. For
-- -- example:
-- --
-- -- > foo = do
-- -- >   let vec = V.generate 10 id
-- -- >   mvec <- V.unsafeThaw vec
-- -- >   do_something mvec
-- --
-- -- Here GHC could lift @vec@ outside of foo which means that all calls to
-- -- @do_something@ will use same buffer with possibly disastrous
-- -- results. Whether such aliasing happens or not depends on the program in
-- -- question, optimization levels, and GHC flags.
-- --
-- -- All in all, attempts to modify a vector produced by @unsafeThaw@ fall out of
-- -- domain of software engineering and into realm of black magic, dark
-- -- rituals, and unspeakable horrors. The only advice that could be given
-- -- is: "Don't attempt to mutate a vector produced by @unsafeThaw@ unless you
-- -- know how to prevent GHC from aliasing buffers accidentally. We don't."
-- unsafeThaw :: Vector a -> ST s (M.STVector s a)
-- {-# INLINE unsafeThaw #-}
-- unsafeThaw (UnsafeVector arr) = GHC.ST (\s -> 
--   case GHC.unsafeThawArray# arr s of
--     (# s', marr #) -> (# s', M.UnsafeSTVector marr #))

-- | /O(n)/ Yield a mutable copy of an immutable vector.
thaw :: VectorSlice a -> ST s (M.STVector s a)
{-# INLINE thaw #-}
thaw (UnsafeVectorSlice (UnsafeVector arr) (GHC.I# i) (GHC.I# n)) = GHC.ST (\s -> 
  case GHC.thawSmallArray# arr i n s of
    (# s', marr #) -> (# s', M.UnsafeSTVector marr #))

-- | /O(n)/ Copy an immutable vector into a mutable one.
unsafeCopy :: VectorSlice a -> M.STVectorSlice s a -> ST s ()
{-# INLINE unsafeCopy #-}
unsafeCopy (UnsafeVectorSlice (UnsafeVector arr) (GHC.I# offv) _) (M.UnsafeSTVectorSlice (M.UnsafeSTVector marr) (GHC.I# offm) (GHC.I# len)) = GHC.ST (\s -> 
  (# GHC.copySmallArray# arr offv marr offm len s , () #))

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
copy :: VectorSlice a -> M.STVectorSlice s a -> ST s ()
{-# INLINE copy #-}
copy v@(UnsafeVectorSlice _ _ vn) m@(M.UnsafeSTVectorSlice _ _ mn)
  | mn == vn = unsafeCopy v m
  | otherwise = error "copy: vector slices have different lengths"

-- -- -- $setup
-- -- -- >>> :set -Wno-type-defaults
-- -- -- >>> import Prelude (Char, String, Bool(True, False), min, max, fst, even, undefined, Ord(..), ($), (<>), Num(..))
