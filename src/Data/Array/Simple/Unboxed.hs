{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-stg-final -dsuppress-all -dno-typeable-binds -dno-suppress-type-signatures -ddump-to-file #-}
{-# LANGUAGE RoleAnnotations, TypeAbstractions #-}
-- |
-- Module      : Data.UArray.Simple
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
-- A library for strict immutable UArrays (that is, polymorphic UArrays capable
-- of holding any Haskell value).
module Data.Array.Simple.Unboxed (
  -- * Boxed Arrays
  UArray, 
  -- MUArray,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  -- -- ** Monadic indexing
  -- indexM, headM, lastM,
  -- unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- -- ** Extracting subUArrays (slicing)
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

  -- -- * Modifying UArrays

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

  -- -- ** UArrays
  -- toUArray, fromUArray, toUArraySlice, unsafeFromUArraySlice,

  -- -- ** Other UArray types
  -- G.convert,

  -- ** Mutable UArrays
  -- freeze, unsafeFreeze, thaw, copy, unsafeCopy,
  -- unsafeThaw, 

  -- ** Grow UArrays
  -- petrify, unsafePetrify,

  -- * Slicing (unstable)
  -- UArraySlice (..), whole, unsafeTakeL, unsafeTakeR, unsafeDropL, unsafeDropR,
) where

import qualified Data.Array.Simple.Unboxed.Mutable as M
import qualified Data.Array.Simple.Unboxed.Grow as G
import qualified Data.Array.Simple.Unboxed.Class as Class
import Data.Array.Simple.Unboxed.Class (Unbox)

import qualified GHC.Exts as GHC

-- import Control.Monad.Primitive
import qualified GHC.ST as GHC
import Control.Monad.ST

import Prelude
  ( Eq (..), Ord (..), Num (..), Monoid (..), Monad (..), Bool, Ordering(..), Int, Maybe
  , (&&), otherwise, error, Maybe (..), Show (..), IO, Foldable, flip, (||), Bool (..), (&&), not
  , fromIntegral, Functor, Semigroup (..), fst, quot)
import qualified Prelude
import Data.Maybe (maybe)
import qualified Data.Foldable as Foldable
import qualified Unsafe.Coerce
import Data.STRef ( readSTRef )

import Data.Array.Byte
import Data.Kind

-- | This UArray type is strict in its elements.
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
-- The memory representation of a 'UArray' is:
--
-- > ╭─────────────┬───╮  ╭────────┬──────┬────────────╮
-- > │ Constructor │ * ┼─➤│ Header │ Size │ Payload... │
-- > ╰─────────────┴───╯  ╰────────┴──────┴────────────╯
--
-- And its overhead is the following:
--
-- * 'UnsafeUArray' constructor: 1 word
-- * Pointer to 'ByteArray#': 1 word
-- * 'ByteArray#' Header: 1 word
-- * 'ByteArray#' Size: 1 word
-- * Payload: sizeOf a bytes per element (rounded up to multiple of word size)
--
-- Where a word is the unit of heap allocation,
-- measuring 8 bytes on 64-bit systems, and 4 bytes on 32-bit systems.
type UArray :: Type -> Type
type role UArray nominal
newtype UArray a = UnsafeUArray ByteArray

instance (Unbox a, Eq a) => Eq (UArray a) where
  (==) = eqBy (==)

instance (Unbox a, Ord a) => Ord (UArray a) where
  compare = cmpBy compare

instance (Unbox a, Show a) => Show (UArray a) where
  showsPrec p x = showsPrec p (toList x)

-- Length information
-- ------------------

-- | /O(1)/ Yield the length of the UArray.
length :: Unbox a => UArray a -> Int
{-# INLINE length #-}
length @a (UnsafeUArray (ByteArray arr)) = GHC.I# (GHC.sizeofByteArray# arr) `quot` Class.sizeOf a

-- | /O(1)/ Test whether a UArray is empty.
null :: Unbox a => UArray a -> Bool
{-# INLINE null #-}
null v = length v == 0

-- Indexing
-- --------

-- | O(1) Indexing.
(!) :: Unbox a => UArray a -> Int -> a
{-# INLINE (!) #-}
v ! i
  | 0 <= i && i < length v = unsafeIndex v i
  | otherwise = error ("!: index out of bounds")

-- | O(1) Safe indexing.
(!?) :: Unbox a => UArray a -> Int -> Maybe a
{-# INLINE (!?) #-}
v !? i
  | 0 <= i && i < length v = let !x = unsafeIndex v i in Just x
  | otherwise = Nothing

-- | /O(1)/ First element.
head :: Unbox a => UArray a -> a
{-# INLINE head #-}
head v = v ! 0

-- | /O(1)/ Last element.
last :: Unbox a => UArray a -> a
{-# INLINE last #-}
last v = v ! (length v - 1)

-- | /O(1)/ Unsafe indexing without bounds checking.
unsafeIndex :: Unbox a => UArray a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex (UnsafeUArray ba) i = Class.unsafeIndex ba i

-- | /O(1)/ First element, without checking if the UArray is empty.
unsafeHead :: Unbox a => UArray a -> a
{-# INLINE unsafeHead #-}
unsafeHead v = unsafeIndex v 0

-- | /O(1)/ Last element, without checking if the UArray is empty.
unsafeLast :: Unbox a => UArray a -> a
{-# INLINE unsafeLast #-}
unsafeLast v = unsafeIndex v (length v - 1)

-- -- Extracting subUArrays (slicing)
-- -- -------------------------------

-- -- | /O(n)/ Yield a slice of the UArray by copying it. The UArray must
-- -- contain at least @i+n@ elements.
-- slice :: Int   -- ^ @i@ starting index
--       -> Int   -- ^ @n@ length
--       -> UArray a
--       -> UArray a
-- {-# INLINE slice #-}
-- slice i n v
--   | 0 <= i && 0 < n && i + n < length v = unsafeSlice i n v
--   | otherwise = error ("Slice arguments out of bounds: " <> show (i, n))

-- -- | /O(1)/ Yield a slice of the UArray without copying. The UArray must
-- -- contain at least @i+n@ elements, but this is not checked.
-- unsafeSlice :: Int   -- ^ @i@ starting index
--             -> Int   -- ^ @n@ length
--             -> UArray a
--             -> UArray a
-- unsafeSlice i n v = runST (do
--   m <- M.new n
--   unsafeCopy (unsafeDropL i (whole v)) (M.whole m)
--   unsafeFreeze m)

-- Initialisation
-- --------------

-- | /O(1)/ The empty UArray.
empty :: Unbox a => UArray a
{-# INLINE empty #-}
empty = runST (do m <- M.new 0; unsafeFreeze m)

-- | /O(1)/ A UArray with exactly one element.
singleton :: Unbox a => a -> UArray a
{-# INLINE singleton #-}
singleton = replicate 1

-- | /O(n)/ A UArray of the given length with the same value in each position.
replicate :: Unbox a => Int -> a -> UArray a
{-# INLINE replicate #-}
replicate n x = runST (do 
  m <- M.new n
  let
    go i 
      | i < n = M.write m i x
      | otherwise = return ()
  go 0
  unsafeFreeze m)

-- | /O(n)/ Construct a UArray of the given length by applying the function to
-- each index.
generate :: Unbox a => Int -> (Int -> a) -> UArray a
{-# INLINE generate #-}
generate n f = iunfoldrExactN n (\i _ -> let !x = f i in (x, ())) ()

-- | /O(n)/ Apply the function \(\max(n - 1, 0)\) times to an initial value, producing a UArray
-- of length \(\max(n, 0)\). The 0th element will contain the initial value, which is why there
-- is one less function application than the number of elements in the produced UArray.
--
-- \( \underbrace{x, f (x), f (f (x)), \ldots}_{\max(0,n)\rm{~elements}} \)
--
-- ===__Examples__
--
-- >>> import qualified Data.UArray as V
-- >>> V.iterateN 0 undefined undefined :: V.UArray String
-- []
-- >>> V.iterateN 4 (\x -> x <> x) "Hi"
-- ["Hi","HiHi","HiHiHiHi","HiHiHiHiHiHiHiHi"]
iterateN :: Unbox a => Int -> (a -> a) -> a -> UArray a
{-# INLINE iterateN #-}
iterateN n f x0 = unfoldrExactN n (\x -> (x, f x)) x0

-- -- Unfolding
-- -- ---------

-- TODO: this can be implemented when we have growable UArrays in the future:
-- -- | /O(n)/ Construct a UArray by repeatedly applying the generator function
-- -- to a seed. The generator function yields 'Just' the next element and the
-- -- new seed or 'Nothing' if there are no more elements.
-- --
-- -- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- -- >  = <10,9,8,7,6,5,4,3,2,1>
-- unfoldr :: (b -> Maybe (a, b)) -> b -> UArray a
-- {-# INLINE unfoldr #-}
-- unfoldr 

-- | /O(n)/ Construct a UArray with at most @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN :: Unbox a => Int -> (b -> Maybe (a, b)) -> b -> UArray a
{-# INLINE unfoldrN #-}
unfoldrN n f x = iunfoldrN n (\_ -> f) x

-- | /O(n)/ Construct a UArray with exactly @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields the
-- next element and the new seed.
--
-- > unfoldrExactN 3 (\n -> (n,n-1)) 10 = <10,9,8>
unfoldrExactN :: Unbox a => Int -> (b -> (a, b)) -> b -> UArray a
{-# INLINE unfoldrExactN #-}
unfoldrExactN n f x0 = unfoldrN n (\x -> let !y = f x in Just y) x0

-- -- -- | /O(n)/ Construct a UArray by repeatedly applying the monadic
-- -- -- generator function to a seed. The generator function yields 'Just'
-- -- -- the next element and the new seed or 'Nothing' if there are no more
-- -- -- elements.
-- -- unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m (UArray a)
-- -- {-# INLINE unfoldrM #-}
-- -- unfoldrM = G.unfoldrM

-- -- | /O(n)/ Construct a UArray by repeatedly applying the monadic
-- -- generator function to a seed. The generator function yields 'Just'
-- -- the next element and the new seed or 'Nothing' if there are no more
-- -- elements.
-- unfoldrNST :: Int -> (b -> ST s (Maybe (a, b))) -> b -> ST s (UArray a)
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

-- | /O(n)/ Construct a UArray with at most @n@ elements by repeatedly applying
-- the generator function to the current index and a seed. The generator 
-- function yields 'Just' the next element and the new seed or 'Nothing' if 
-- there are no more elements.
iunfoldrN :: Unbox a => Int -> (Int -> b -> Maybe (a, b)) -> b -> UArray a
{-# INLINE iunfoldrN #-}
iunfoldrN n f !x0 = runST (do
  m <- M.new n
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

-- | /O(n)/ Construct a UArray with exactly @n@ elements by repeatedly applying
-- the generator function to the current index and a seed. The generator
-- function yields the next element and the new seed.
iunfoldrExactN :: Unbox a => Int -> (Int -> b -> (a, b)) -> b -> UArray a
{-# INLINE iunfoldrExactN #-}
iunfoldrExactN n f x0 = iunfoldrN n (\i x -> Just (f i x)) x0

-- -- | /O(n)/ Construct a UArray with exactly @n@ elements by repeatedly
-- -- applying the monadic generator function to a seed. The generator
-- -- function yields the next element and the new seed.
-- --
-- -- @since 0.12.2.0
-- unfoldrExactNST :: Int -> (b -> ST s (a, b)) -> b -> ST s (UArray a)
-- {-# INLINE unfoldrExactNST #-}
-- unfoldrExactNST n f x0 = unfoldrNST n (\x -> do y <- f x; return (Just y)) x0

-- TODO: SPECIALIZE prevents inlining, reconsider all uses of it.

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

-- -- | /O(n)/ Construct a UArray with @n@ elements by repeatedly applying the
-- -- generator function to the already constructed part of the UArray.
-- --
-- -- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in <a,b,c>
-- constructN :: Int -> (UArray a -> a) -> UArray a
-- {-# INLINE constructN #-}
-- constructN = G.constructN

-- -- | /O(n)/ Construct a UArray with @n@ elements from right to left by
-- -- repeatedly applying the generator function to the already constructed part
-- -- of the UArray.
-- --
-- -- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in <c,b,a>
-- constructrN :: Int -> (UArray a -> a) -> UArray a
-- {-# INLINE constructrN #-}
-- constructrN = G.constructrN

-- -- Enumeration
-- -- -----------

-- | /O(n)/ Yield a UArray of the given length, containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: (Unbox a, Num a) => a -> Int -> UArray a
{-# INLINE enumFromN #-}
enumFromN x0 n = generate n (\i -> x0 + fromIntegral i)

-- | /O(n)/ Yield a UArray of the given length, containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 2 5 = <1,3,5,7,9>
enumFromStepN :: (Unbox a, Num a) => a -> a -> Int -> UArray a
{-# INLINE enumFromStepN #-}
enumFromStepN x0 y n = generate n (\i -> x0 + y * fromIntegral i)

-- -- | /O(n)/ Enumerate values from @x@ to @y@.
-- --
-- -- /WARNING:/ This operation can be very inefficient. If possible, use
-- -- 'enumFromN' instead.
-- enumFromTo :: Enum a => a -> a -> UArray a
-- {-# INLINE enumFromTo #-}
-- enumFromTo = G.enumFromTo

-- -- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
-- --
-- -- /WARNING:/ This operation can be very inefficient. If possible, use
-- -- 'enumFromStepN' instead.
-- enumFromThenTo :: Enum a => a -> a -> a -> UArray a
-- {-# INLINE enumFromThenTo #-}
-- enumFromThenTo = G.enumFromThenTo

-- -- Concatenation
-- -- -------------

-- -- | /O(n)/ Prepend an element.
-- cons :: a -> UArray a -> UArray a
-- {-# INLINE cons #-}
-- cons = G.cons

-- -- | /O(n)/ Append an element.
-- snoc :: UArray a -> a -> UArray a
-- {-# INLINE snoc #-}
-- snoc = G.snoc

infixr 5 ++
-- | /O(m+n)/ Concatenate two UArrays.
(++) :: Unbox a => UArray a -> UArray a -> UArray a
{-# INLINE (++) #-}
v ++ w = generate (length v + length w) (\i -> if i < length v then unsafeIndex v i else unsafeIndex w i)

-- | /O(n)/ Concatenate all UArrays in the list.
-- TODO: this could probably be done in a fusible way
concat :: Unbox a => [UArray a] -> UArray a
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
-- -- results in a UArray.
-- replicateM :: Monad m => Int -> m a -> m (UArray a)
-- {-# INLINE replicateM #-}
-- replicateM n m = unfoldrExactNM n (\() -> do x <- m; return (x, ())) ()
-- {-# SPECIALIZE replicateM :: Int -> IO a -> IO (UArray a) #-}

-- -- | /O(n)/ Construct a UArray of the given length by applying the monadic
-- -- action to each index.
-- generateM :: Monad m => Int -> (Int -> m a) -> m (UArray a)
-- {-# INLINE generateM #-}
-- generateM n f = iunfoldrNM n (\i () -> do x <- f i; return (Just (x, ()))) ()
-- {-# SPECIALIZE generateM :: Int -> (Int -> IO a) -> IO (UArray a) #-}

-- -- | /O(n)/ Apply the monadic function \(\max(n - 1, 0)\) times to an initial value, producing a UArray
-- -- of length \(\max(n, 0)\). The 0th element will contain the initial value, which is why there
-- -- is one less function application than the number of elements in the produced UArray.
-- --
-- -- For a non-monadic version, see `iterateN`.
-- --
-- -- @since 0.12.0.0
-- iterateNM :: Monad m => Int -> (a -> m a) -> a -> m (UArray a)
-- {-# INLINE iterateNM #-}
-- -- TODO: this doesn't produce optimal Core:
-- iterateNM n f x0 = unfoldrNM n (\ !x -> do x' <- f x; return (x' `seq` Just (x, x'))) x0
-- -- TODO: all monadic functions should have specialize for IO:
-- {-# SPECIALIZE iterateNM :: Int -> (a -> IO a) -> a -> IO (UArray a) #-}
-- -- TODO: consider if we really want this:
-- {-# SPECIALIZE iterateNM :: Int -> (a -> Identity a) -> a -> Identity (UArray a) #-}

-- -- -- | Execute the monadic action and freeze the resulting UArray.
-- -- --
-- -- -- @
-- -- -- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- -- -- @
-- -- create :: (forall s. ST s (MUArray s a)) -> UArray a
-- -- {-# INLINE create #-}
-- -- -- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
-- -- create p = G.create p

-- -- -- | Execute the monadic action and freeze the resulting UArrays.
-- -- createT :: Traversable.Traversable f => (forall s. ST s (f (MUArray s a))) -> f (UArray a)
-- -- {-# INLINE createT #-}
-- -- createT p = G.createT p



-- -- -- Restricting memory usage
-- -- -- ------------------------

-- -- -- | /O(n)/ Yield the argument, but force it not to retain any extra memory,
-- -- -- by copying it.
-- -- --
-- -- -- This is especially useful when dealing with slices. For example:
-- -- --
-- -- -- > force (slice 0 2 <huge UArray>)
-- -- --
-- -- -- Here, the slice retains a reference to the huge UArray. Forcing it creates
-- -- -- a copy of just the elements that belong to the slice and allows the huge
-- -- -- UArray to be garbage collected.
-- -- force :: UArray a -> UArray a
-- -- {-# INLINE force #-}
-- -- force = G.force

-- -- Bulk updates
-- -- ------------

-- -- | /O(m+n)/ For each pair @(i,a)@ from the list of index/value pairs,
-- -- replace the UArray element at position @i@ by @a@.
-- --
-- -- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
-- --
-- (//) :: UArray a   -- ^ initial UArray (of length @m@)
--                 -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
--                 -> UArray a
-- {-# INLINE (//) #-}
-- (//) = (G.//)

-- -- | /O(m+n)/ For each pair @(i,a)@ from the UArray of index/value pairs,
-- -- replace the UArray element at position @i@ by @a@.
-- --
-- -- > update <5,9,2,7> <(2,1),(0,3),(2,8)> = <3,9,8,7>
-- --
-- update :: UArray a        -- ^ initial UArray (of length @m@)
--        -> UArray (Int, a) -- ^ UArray of index/value pairs (of length @n@)
--        -> UArray a
-- {-# INLINE update #-}
-- update = G.update

-- -- | /O(m+min(n1,n2))/ For each index @i@ from the index UArray and the
-- -- corresponding value @a@ from the value UArray, replace the element of the
-- -- initial UArray at position @i@ by @a@.
-- --
-- -- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
-- --
-- -- The function 'update' provides the same functionality and is usually more
-- -- convenient.
-- --
-- -- @
-- -- update_ xs is ys = 'update' xs ('zip' is ys)
-- -- @
-- update_ :: UArray a   -- ^ initial UArray (of length @m@)
--         -> UArray Int -- ^ index UArray (of length @n1@)
--         -> UArray a   -- ^ value UArray (of length @n2@)
--         -> UArray a
-- {-# INLINE update_ #-}
-- update_ = G.update_

-- -- | Same as ('//'), but without bounds checking.
-- unsafeUpd :: UArray a -> [(Int, a)] -> UArray a
-- {-# INLINE unsafeUpd #-}
-- unsafeUpd = G.unsafeUpd

-- -- | Same as 'update', but without bounds checking.
-- unsafeUpdate :: UArray a -> UArray (Int, a) -> UArray a
-- {-# INLINE unsafeUpdate #-}
-- unsafeUpdate = G.unsafeUpdate

-- -- | Same as 'update_', but without bounds checking.
-- unsafeUpdate_ :: UArray a -> UArray Int -> UArray a -> UArray a
-- {-# INLINE unsafeUpdate_ #-}
-- unsafeUpdate_ = G.unsafeUpdate_

-- -- Accumulations
-- -- -------------

-- -- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the UArray element
-- -- @a@ at position @i@ by @f a b@.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.accum (+) (V.fromList [1000,2000,3000]) [(2,4),(1,6),(0,3),(1,10)]
-- -- [1003,2016,3004]
-- accum :: (a -> b -> a) -- ^ accumulating function @f@
--       -> UArray a      -- ^ initial UArray (of length @m@)
--       -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
--       -> UArray a
-- {-# INLINE accum #-}
-- accum = G.accum

-- -- | /O(m+n)/ For each pair @(i,b)@ from the UArray of pairs, replace the UArray
-- -- element @a@ at position @i@ by @f a b@.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.accumulate (+) (V.fromList [1000,2000,3000]) (V.fromList [(2,4),(1,6),(0,3),(1,10)])
-- -- [1003,2016,3004]
-- accumulate :: (a -> b -> a)  -- ^ accumulating function @f@
--             -> UArray a       -- ^ initial UArray (of length @m@)
--             -> UArray (Int,b) -- ^ UArray of index/value pairs (of length @n@)
--             -> UArray a
-- {-# INLINE accumulate #-}
-- accumulate = G.accumulate

-- -- | /O(m+min(n1,n2))/ For each index @i@ from the index UArray and the
-- -- corresponding value @b@ from the value UArray,
-- -- replace the element of the initial UArray at
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
--             -> UArray a      -- ^ initial UArray (of length @m@)
--             -> UArray Int    -- ^ index UArray (of length @n1@)
--             -> UArray b      -- ^ value UArray (of length @n2@)
--             -> UArray a
-- {-# INLINE accumulate_ #-}
-- accumulate_ = G.accumulate_

-- -- | Same as 'accum', but without bounds checking.
-- unsafeAccum :: (a -> b -> a) -> UArray a -> [(Int,b)] -> UArray a
-- {-# INLINE unsafeAccum #-}
-- unsafeAccum = G.unsafeAccum

-- -- | Same as 'accumulate', but without bounds checking.
-- unsafeAccumulate :: (a -> b -> a) -> UArray a -> UArray (Int,b) -> UArray a
-- {-# INLINE unsafeAccumulate #-}
-- unsafeAccumulate = G.unsafeAccumulate

-- -- | Same as 'accumulate_', but without bounds checking.
-- unsafeAccumulate_
--   :: (a -> b -> a) -> UArray a -> UArray Int -> UArray b -> UArray a
-- {-# INLINE unsafeAccumulate_ #-}
-- unsafeAccumulate_ = G.unsafeAccumulate_

-- -- Permutations
-- -- ------------

-- | /O(n)/ Reverse a UArray.
reverse :: Unbox a => UArray a -> UArray a
{-# INLINE reverse #-}
reverse v = generate (length v) (\i -> unsafeIndex v (length v - 1 - i))

-- | /O(n)/ Yield the UArray obtained by replacing each element @i@ of the
-- index UArray by @xs'!'i@. This is equivalent to @'map' (xs'!') is@, but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: Unbox a => UArray a -> UArray Int -> UArray a
{-# INLINE backpermute #-}
backpermute vx vi = generate (length vi) (\i -> vx ! unsafeIndex vi i)

-- | Same as 'backpermute', but without bounds checking.
unsafeBackpermute :: Unbox a => UArray a -> UArray Int -> UArray a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute vx vi = generate (length vi) (\i -> unsafeIndex vx (unsafeIndex vi i))

-- -- Safe destructive updates
-- -- ------------------------

-- -- | Apply a destructive operation to a UArray. The operation may be
-- -- performed in place if it is safe to do so and will modify a copy of the
-- -- UArray otherwise (see 'Data.UArray.Generic.New.New' for details).
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.UArray as V
-- -- >>> import qualified Data.UArray.Mutable as MV
-- -- >>> V.modify (\v -> MV.write v 0 'x') $ V.replicate 4 'a'
-- -- "xaaa"
-- modify :: (forall s. MUArray s a -> ST s ()) -> UArray a -> UArray a
-- {-# INLINE modify #-}
-- modify p = G.modify p

-- -- Indexing
-- -- --------

-- TODO: this is probably not worth it without fusion:
--
-- -- | /O(n)/ Pair each element in a UArray with its index.
-- indexed :: UArray a -> UArray (Int,a)
-- {-# INLINE indexed #-}
-- indexed = G.indexed

-- Mapping
-- -------

-- | /O(n)/ Map a function over a UArray.
-- Warning: does not fuse, this will allocate a new copy of the UArray.
-- Consider using explicit streaming (TODO) if you compose this with other combinators.
map :: (Unbox a, Unbox b) => (a -> b) -> UArray a -> UArray b
{-# INLINE map #-}
map f v = imap (\_ -> f) v

-- | /O(n)/ Apply a function to every element of a UArray and its index.
imap :: (Unbox a, Unbox b) => (Int -> a -> b) -> UArray a -> UArray b
{-# INLINE imap #-}
imap f v = generate (length v) (\i -> let !x = unsafeIndex v i in f i x)

-- -- | Map a function over a UArray and concatenate the results.
-- concatMap :: (a -> UArray b) -> UArray a -> UArray b
-- {-# INLINE concatMap #-}
-- concatMap = G.concatMap

-- -- | Map a function to every element of a UArray and its index, and concatenate the results.
-- --
-- -- @since 0.13.3.0
-- iconcatMap :: (Int -> a -> UArray b) -> UArray a -> UArray b
-- {-# INLINE iconcatMap #-}
-- iconcatMap = G.iconcatMap

-- -- Monadic mapping
-- -- ---------------

-- -- | /O(n)/ Apply the monadic action to all elements of the UArray, yielding a
-- -- UArray of results.
-- mapM :: Monad m => (a -> m b) -> UArray a -> m (UArray b)
-- {-# INLINE mapM #-}
-- mapM f v = _
  
--  -- iunfoldrNM (length v) (\i () -> let !x = unsafeIndex v i in do y <- f x; return (Just (y, ()))) ()
-- {-# SPECIALIZE mapM :: (a -> IO b) -> UArray a -> IO (UArray b) #-}

-- -- | /O(n)/ Apply the monadic action to every element of a UArray and its
-- -- index, yielding a UArray of results.
-- imapM :: Monad m => (Int -> a -> m b) -> UArray a -> m (UArray b)
-- {-# INLINE imapM #-}
-- imapM f v = iunfoldrNM (length v) (\i () -> let !x = unsafeIndex v i in do y <- f i x; return (Just (y, ()))) ()
-- {-# SPECIALIZE imapM :: (Int -> a -> IO b) -> UArray a -> IO (UArray b) #-}

-- | /O(n)/ Apply the monadic action to all elements of a UArray and ignore the
-- results.
mapM_ :: (Unbox a, Monad m) => (a -> m b) -> UArray a -> m ()
{-# INLINE mapM_ #-}
mapM_ f = imapM_ (\_ -> f)

-- | /O(n)/ Apply the monadic action to every element of a UArray and its
-- index, ignoring the results.
imapM_ :: (Unbox a, Monad m) => (Int -> a -> m b) -> UArray a -> m ()
{-# INLINE imapM_ #-}
imapM_ f = ifoldr (\ !i !x xs -> f i x >> xs) (return ())

-- | /O(n)/ Apply the monadic action to all elements of a UArray and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: (Unbox a, Monad m) => UArray a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ v f = mapM_ f v

-- | /O(n)/ Apply the monadic action to all elements of the UArray and their indices
-- and ignore the results. Equivalent to @'flip' 'imapM_'@.
iforM_ :: (Unbox a, Monad m) => UArray a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ v f = imapM_ f v

-- -- Zipping
-- -- -------

-- -- | /O(min(m,n))/ Zip two UArrays with the given function.
-- zipWith :: (a -> b -> c) -> UArray a -> UArray b -> UArray c
-- {-# INLINE zipWith #-}
-- zipWith = G.zipWith

-- -- | Zip three UArrays with the given function.
-- zipWith3 :: (a -> b -> c -> d) -> UArray a -> UArray b -> UArray c -> UArray d
-- {-# INLINE zipWith3 #-}
-- zipWith3 = G.zipWith3

-- zipWith4 :: (a -> b -> c -> d -> e)
--          -> UArray a -> UArray b -> UArray c -> UArray d -> UArray e
-- {-# INLINE zipWith4 #-}
-- zipWith4 = G.zipWith4

-- zipWith5 :: (a -> b -> c -> d -> e -> f)
--          -> UArray a -> UArray b -> UArray c -> UArray d -> UArray e
--          -> UArray f
-- {-# INLINE zipWith5 #-}
-- zipWith5 = G.zipWith5

-- zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
--          -> UArray a -> UArray b -> UArray c -> UArray d -> UArray e
--          -> UArray f -> UArray g
-- {-# INLINE zipWith6 #-}
-- zipWith6 = G.zipWith6

-- -- | /O(min(m,n))/ Zip two UArrays with a function that also takes the
-- -- elements' indices.
-- izipWith :: (Int -> a -> b -> c) -> UArray a -> UArray b -> UArray c
-- {-# INLINE izipWith #-}
-- izipWith = G.izipWith

-- -- | Zip three UArrays and their indices with the given function.
-- izipWith3 :: (Int -> a -> b -> c -> d)
--           -> UArray a -> UArray b -> UArray c -> UArray d
-- {-# INLINE izipWith3 #-}
-- izipWith3 = G.izipWith3

-- izipWith4 :: (Int -> a -> b -> c -> d -> e)
--           -> UArray a -> UArray b -> UArray c -> UArray d -> UArray e
-- {-# INLINE izipWith4 #-}
-- izipWith4 = G.izipWith4

-- izipWith5 :: (Int -> a -> b -> c -> d -> e -> f)
--           -> UArray a -> UArray b -> UArray c -> UArray d -> UArray e
--           -> UArray f
-- {-# INLINE izipWith5 #-}
-- izipWith5 = G.izipWith5

-- izipWith6 :: (Int -> a -> b -> c -> d -> e -> f -> g)
--           -> UArray a -> UArray b -> UArray c -> UArray d -> UArray e
--           -> UArray f -> UArray g
-- {-# INLINE izipWith6 #-}
-- izipWith6 = G.izipWith6

-- -- | /O(min(m,n))/ Zip two UArrays.
-- zip :: UArray a -> UArray b -> UArray (a, b)
-- {-# INLINE zip #-}
-- zip = G.zip

-- -- | Zip together three UArrays into a UArray of triples.
-- zip3 :: UArray a -> UArray b -> UArray c -> UArray (a, b, c)
-- {-# INLINE zip3 #-}
-- zip3 = G.zip3

-- zip4 :: UArray a -> UArray b -> UArray c -> UArray d
--      -> UArray (a, b, c, d)
-- {-# INLINE zip4 #-}
-- zip4 = G.zip4

-- zip5 :: UArray a -> UArray b -> UArray c -> UArray d -> UArray e
--      -> UArray (a, b, c, d, e)
-- {-# INLINE zip5 #-}
-- zip5 = G.zip5

-- zip6 :: UArray a -> UArray b -> UArray c -> UArray d -> UArray e -> UArray f
--      -> UArray (a, b, c, d, e, f)
-- {-# INLINE zip6 #-}
-- zip6 = G.zip6

-- -- Unzipping
-- -- ---------

-- -- | /O(min(m,n))/ Unzip a UArray of pairs.
-- unzip :: UArray (a, b) -> (UArray a, UArray b)
-- {-# INLINE unzip #-}
-- unzip = G.unzip

-- unzip3 :: UArray (a, b, c) -> (UArray a, UArray b, UArray c)
-- {-# INLINE unzip3 #-}
-- unzip3 = G.unzip3

-- unzip4 :: UArray (a, b, c, d) -> (UArray a, UArray b, UArray c, UArray d)
-- {-# INLINE unzip4 #-}
-- unzip4 = G.unzip4

-- unzip5 :: UArray (a, b, c, d, e)
--        -> (UArray a, UArray b, UArray c, UArray d, UArray e)
-- {-# INLINE unzip5 #-}
-- unzip5 = G.unzip5

-- unzip6 :: UArray (a, b, c, d, e, f)
--        -> (UArray a, UArray b, UArray c, UArray d, UArray e, UArray f)
-- {-# INLINE unzip6 #-}
-- unzip6 = G.unzip6

-- -- Monadic zipping
-- -- ---------------

-- -- | /O(min(m,n))/ Zip the two UArrays with the monadic action and yield a
-- -- UArray of results.
-- zipWithM :: Monad m => (a -> b -> m c) -> UArray a -> UArray b -> m (UArray c)
-- {-# INLINE zipWithM #-}
-- zipWithM = G.zipWithM

-- -- | /O(min(m,n))/ Zip the two UArrays with a monadic action that also takes
-- -- the element index and yield a UArray of results.
-- izipWithM :: Monad m => (Int -> a -> b -> m c) -> UArray a -> UArray b -> m (UArray c)
-- {-# INLINE izipWithM #-}
-- izipWithM = G.izipWithM

-- -- | /O(min(m,n))/ Zip the two UArrays with the monadic action and ignore the
-- -- results.
-- zipWithM_ :: Monad m => (a -> b -> m c) -> UArray a -> UArray b -> m ()
-- {-# INLINE zipWithM_ #-}
-- zipWithM_ = G.zipWithM_

-- -- | /O(min(m,n))/ Zip the two UArrays with a monadic action that also takes
-- -- the element index and ignore the results.
-- izipWithM_ :: Monad m => (Int -> a -> b -> m c) -> UArray a -> UArray b -> m ()
-- {-# INLINE izipWithM_ #-}
-- izipWithM_ = G.izipWithM_

-- -- Filtering
-- -- ---------

-- -- | /O(n)/ Drop all elements that do not satisfy the predicate.
-- filter :: (a -> Bool) -> UArray a -> UArray a
-- {-# INLINE filter #-}
-- filter = G.filter

-- -- | /O(n)/ Drop all elements that do not satisfy the predicate which is applied to
-- -- the values and their indices.
-- ifilter :: (Int -> a -> Bool) -> UArray a -> UArray a
-- {-# INLINE ifilter #-}
-- ifilter = G.ifilter

-- -- | /O(n)/ Drop repeated adjacent elements. The first element in each group is returned.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.uniq $ V.fromList [1,3,3,200,3]
-- -- [1,3,200,3]
-- -- >>> import Data.Semigroup
-- -- >>> V.uniq $ V.fromList [ Arg 1 'a', Arg 1 'b', Arg 1 'c']
-- -- [Arg 1 'a']
-- uniq :: (Eq a) => UArray a -> UArray a
-- {-# INLINE uniq #-}
-- uniq = G.uniq

-- -- | /O(n)/ Map the values and collect the 'Just' results.
-- mapMaybe :: (a -> Maybe b) -> UArray a -> UArray b
-- {-# INLINE mapMaybe #-}
-- mapMaybe = G.mapMaybe

-- -- | /O(n)/ Map the indices/values and collect the 'Just' results.
-- imapMaybe :: (Int -> a -> Maybe b) -> UArray a -> UArray b
-- {-# INLINE imapMaybe #-}
-- imapMaybe = G.imapMaybe

-- -- | /O(n)/ Return a UArray of all the 'Just' values.
-- --
-- -- @since 0.12.2.0
-- catMaybes :: UArray (Maybe a) -> UArray a
-- {-# INLINE catMaybes #-}
-- catMaybes = mapMaybe id

-- -- | /O(n)/ Drop all elements that do not satisfy the monadic predicate.
-- filterM :: Monad m => (a -> m Bool) -> UArray a -> m (UArray a)
-- {-# INLINE filterM #-}
-- filterM = G.filterM

-- -- | /O(n)/ Apply the monadic function to each element of the UArray and
-- -- discard elements returning 'Nothing'.
-- --
-- -- @since 0.12.2.0
-- mapMaybeM :: Monad m => (a -> m (Maybe b)) -> UArray a -> m (UArray b)
-- {-# INLINE mapMaybeM #-}
-- mapMaybeM = G.mapMaybeM

-- -- | /O(n)/ Apply the monadic function to each element of the UArray and its index.
-- -- Discard elements returning 'Nothing'.
-- --
-- -- @since 0.12.2.0
-- imapMaybeM :: Monad m => (Int -> a -> m (Maybe b)) -> UArray a -> m (UArray b)
-- {-# INLINE imapMaybeM #-}
-- imapMaybeM = G.imapMaybeM

-- -- | /O(n)/ Yield the longest prefix of elements satisfying the predicate.
-- -- The current implementation is not copy-free, unless the result UArray is
-- -- fused away.
-- takeWhile :: (a -> Bool) -> UArray a -> UArray a
-- {-# INLINE takeWhile #-}
-- takeWhile = G.takeWhile

-- -- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- -- without copying.
-- dropWhile :: (a -> Bool) -> UArray a -> UArray a
-- {-# INLINE dropWhile #-}
-- dropWhile = G.dropWhile

-- -- Parititioning
-- -- -------------

-- -- | /O(n)/ Split the UArray in two parts, the first one containing those
-- -- elements that satisfy the predicate and the second one those that don't. The
-- -- relative order of the elements is preserved at the cost of a sometimes
-- -- reduced performance compared to 'unstablePartition'.
-- partition :: (a -> Bool) -> UArray a -> (UArray a, UArray a)
-- {-# INLINE partition #-}
-- partition = G.partition

-- -- | /O(n)/ Split the UArray into two parts, the first one containing the
-- -- @`Left`@ elements and the second containing the @`Right`@ elements.
-- -- The relative order of the elements is preserved.
-- --
-- -- @since 0.12.1.0
-- partitionWith :: (a -> Either b c) -> UArray a -> (UArray b, UArray c)
-- {-# INLINE partitionWith #-}
-- partitionWith = G.partitionWith

-- -- | /O(n)/ Split the UArray in two parts, the first one containing those
-- -- elements that satisfy the predicate and the second one those that don't.
-- -- The order of the elements is not preserved, but the operation is often
-- -- faster than 'partition'.
-- unstablePartition :: (a -> Bool) -> UArray a -> (UArray a, UArray a)
-- {-# INLINE unstablePartition #-}
-- unstablePartition = G.unstablePartition

-- -- | /O(n)/ Split the UArray into the longest prefix of elements that satisfy
-- -- the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.span (<4) $ V.generate 10 id
-- -- ([0,1,2,3],[4,5,6,7,8,9])
-- span :: (a -> Bool) -> UArray a -> (UArray a, UArray a)
-- {-# INLINE span #-}
-- span = G.span

-- -- | /O(n)/ Split the UArray into the longest prefix of elements that do not
-- -- satisfy the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.break (>4) $ V.generate 10 id
-- -- ([0,1,2,3,4],[5,6,7,8,9])
-- break :: (a -> Bool) -> UArray a -> (UArray a, UArray a)
-- {-# INLINE break #-}
-- break = G.break

-- -- | /O(n)/ Split the UArray into the longest prefix of elements that satisfy
-- -- the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.spanR (>4) $ V.generate 10 id
-- -- ([5,6,7,8,9],[0,1,2,3,4])
-- --
-- -- @since 0.13.2.0
-- spanR :: (a -> Bool) -> UArray a -> (UArray a, UArray a)
-- {-# INLINE spanR #-}
-- spanR = G.spanR

-- -- | /O(n)/ Split the UArray into the longest prefix of elements that do not
-- -- satisfy the predicate and the rest without copying.
-- --
-- -- Does not fuse.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.breakR (<5) $ V.generate 10 id
-- -- ([5,6,7,8,9],[0,1,2,3,4])
-- --
-- -- @since 0.13.2.0
-- breakR :: (a -> Bool) -> UArray a -> (UArray a, UArray a)
-- {-# INLINE breakR #-}
-- breakR = G.breakR

-- -- | /O(n)/ Split a UArray into a list of slices, using a predicate function.
-- --
-- -- The concatenation of this list of slices is equal to the argument UArray,
-- -- and each slice contains only equal elements, as determined by the equality
-- -- predicate function.
-- --
-- -- Does not fuse.
-- --
-- -- >>> import qualified Data.UArray as V
-- -- >>> import           Data.Char (isUpper)
-- -- >>> V.groupBy (\a b -> isUpper a == isUpper b) (V.fromList "Mississippi River")
-- -- ["M","ississippi ","R","iver"]
-- --
-- -- See also 'Data.List.groupBy', 'group'.
-- --
-- -- @since 0.13.0.0
-- groupBy :: (a -> a -> Bool) -> UArray a -> [UArray a]
-- {-# INLINE groupBy #-}
-- groupBy = G.groupBy

-- -- | /O(n)/ Split a UArray into a list of slices of the input UArray.
-- --
-- -- The concatenation of this list of slices is equal to the argument UArray,
-- -- and each slice contains only equal elements.
-- --
-- -- Does not fuse.
-- --
-- -- This is the equivalent of 'groupBy (==)'.
-- --
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.group (V.fromList "Mississippi")
-- -- ["M","i","ss","i","ss","i","pp","i"]
-- --
-- -- See also 'Data.List.group'.
-- --
-- -- @since 0.13.0.0
-- group :: Eq a => UArray a -> [UArray a]
-- {-# INLINE group #-}
-- group = G.groupBy (==)

-- -- Searching
-- -- ---------

infix 4 `elem`
-- | /O(n)/ Check if the UArray contains an element.
elem :: (Unbox a, Eq a) => a -> UArray a -> Bool
{-# INLINE elem #-}
elem z = foldr (\ x xs -> x == z || xs) False

infix 4 `notElem`
-- | /O(n)/ Check if the UArray does not contain an element (inverse of 'elem').
notElem :: (Unbox a, Eq a) => a -> UArray a -> Bool
{-# INLINE notElem #-}
notElem z v = not (elem z v)

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: Unbox a => (a -> Bool) -> UArray a -> Maybe a
{-# INLINE find #-}
find f = foldr (\x xs -> if f x then Just x else xs) Nothing

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: Unbox a => (a -> Bool) -> UArray a -> Maybe Int
{-# INLINE findIndex #-}
findIndex f = ifoldr (\i x xs -> if f x then Just i else xs) Nothing

-- -- | /O(n)/ Yield 'Just' the index of the /last/ element matching the predicate
-- -- or 'Nothing' if no such element exists.
-- --
-- -- Does not fuse.
-- findIndexR :: (a -> Bool) -> UArray a -> Maybe Int
-- {-# INLINE findIndexR #-}
-- findIndexR = G.findIndexR

-- -- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- -- order.
-- findIndices :: (a -> Bool) -> UArray a -> UArray Int
-- {-# INLINE findIndices #-}
-- findIndices = G.findIndices

-- | /O(n)/ Yield 'Just' the index of the first occurrence of the given element or
-- 'Nothing' if the UArray does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (Unbox a, Eq a) => a -> UArray a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex x = findIndex (== x)

-- -- | /O(n)/ Yield the indices of all occurrences of the given element in
-- -- ascending order. This is a specialised version of 'findIndices'.
-- elemIndices :: Eq a => a -> UArray a -> UArray Int
-- {-# INLINE elemIndices #-}
-- elemIndices = G.elemIndices

-- -- Folding
-- -- -------

-- | /O(n)/ Left fold.
foldl :: Unbox b => (a -> b -> a) -> a -> UArray b -> a
{-# INLINE foldl #-}
foldl k z v = go z 0 where
  go s i
    | i < length v = let !x = unsafeIndex v i in go (k s x) (i + 1)
    | otherwise = s

-- | /O(n)/ Left fold on non-empty UArrays.
foldl1 :: Unbox a => (a -> a -> a) -> UArray a -> a
{-# INLINE foldl1 #-}
foldl1 k v 
  | null v = error "foldr1 applied to empty UArray"
  | otherwise = go (unsafeIndex v 0) 1 where
  go s i
    | i < length v - 1 = let !x = unsafeIndex v i in go (k s x) (i + 1)
    | otherwise = unsafeIndex v (length v - 1)

-- | /O(n)/ Left fold with strict accumulator.
foldl' :: Unbox b => (a -> b -> a) -> a -> UArray b -> a
{-# INLINE foldl' #-}
foldl' k z = \v -> 
  let
    go !s i
      | i < length v = let !x = unsafeIndex v i in go (k s x) (i + 1)
      | otherwise = s
  in go z 0

-- | /O(n)/ Left fold on non-empty UArrays with strict accumulator.
foldl1' :: Unbox a => (a -> a -> a) -> UArray a -> a
{-# INLINE foldl1' #-}
foldl1' k v 
  | null v = error "foldr1 applied to empty UArray"
  | otherwise = go (unsafeIndex v 0) 1 where
  go !s i
    | i < length v - 1 = let !x = unsafeIndex v i in go (k s x) (i + 1)
    | otherwise = unsafeIndex v (length v - 1)

-- | /O(n)/ Right fold.
foldr :: Unbox a => (a -> b -> b) -> b -> UArray a -> b
{-# INLINE foldr #-}
foldr k z = \v -> 
  let
    go i
      | i < length v = let !x = unsafeIndex v i in k x (go (i + 1))
      | otherwise = z
  in go 0

-- TODO: implement as left-to-right pass?
-- | /O(n)/ Right fold on non-empty UArrays.
foldr1 :: Unbox a => (a -> a -> a) -> UArray a -> a
{-# INLINE foldr1 #-}
foldr1 k v 
  | null v = error "foldr1 applied to empty UArray"
  | otherwise = go (unsafeIndex v (length v - 1)) (length v - 2) where
  go s i
    | 0 <= i = let !x = unsafeIndex v i in go (k x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Right fold with a strict accumulator.
foldr' :: Unbox a => (a -> b -> b) -> b -> UArray a -> b
{-# INLINE foldr' #-}
foldr' k z v = go z (length v - 1) where
  go !s i
    | 0 <= i = let !x = unsafeIndex v i in go (k x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Right fold on non-empty UArrays with strict accumulator.
foldr1' :: Unbox a => (a -> a -> a) -> UArray a -> a
{-# INLINE foldr1' #-}
foldr1' k v 
  | null v = error "foldr1 applied to empty UArray"
  | otherwise = go (unsafeIndex v (length v - 1)) (length v - 2) where
  go !s i
    | 0 <= i = let !x = unsafeIndex v i in go (k x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Left fold using a function applied to each element and its index.
ifoldl :: Unbox b => (a -> Int -> b -> a) -> a -> UArray b -> a
{-# INLINE ifoldl #-}
ifoldl k z v = go z 0 where
  go s i
    | i < length v = let !x = unsafeIndex v i in go (k s i x) (i + 1)
    | otherwise = s

-- | /O(n)/ Left fold with strict accumulator using a function applied to each element
-- and its index.
ifoldl' :: Unbox b => (a -> Int -> b -> a) -> a -> UArray b -> a
{-# INLINE ifoldl' #-}
ifoldl' k z v = go z 0 where
  go !s i
    | i < length v = let !x = unsafeIndex v i in go (k s i x) (i + 1)
    | otherwise = s

-- | /O(n)/ Right fold using a function applied to each element and its index.
ifoldr :: Unbox a => (Int -> a -> b -> b) -> b -> UArray a -> b
{-# INLINE ifoldr #-}
ifoldr k z v = go 0 where
  go i
    | i < length v = let !x = unsafeIndex v i in k i x (go (i - 1))
    | otherwise = z

-- | /O(n)/ Right fold with strict accumulator using a function applied to each
-- element and its index.
ifoldr' :: Unbox a => (Int -> a -> b -> b) -> b -> UArray a -> b
{-# INLINE ifoldr' #-}
ifoldr' k z v = go z (length v - 1) where
  go !s i
    | 0 <= i = let !x = unsafeIndex v i in go (k i x s) (i - 1)
    | otherwise = s

-- | /O(n)/ Map each element of the structure to a monoid and combine
-- the results. It uses the same implementation as the corresponding method
-- of the 'Foldable' type class.
foldMap :: (Unbox a, Monoid m) => (a -> m) -> UArray a -> m
{-# INLINE foldMap #-}
foldMap f = foldr (\x m -> f x <> m) mempty

-- | /O(n)/ Like 'foldMap', but strict in the accumulator. It uses the same
-- implementation as the corresponding method of the 'Foldable' type class.
-- Note that it's implemented in terms of 'foldl'', so it fuses in most
-- contGHC.
foldMap' :: (Unbox a, Monoid m) => (a -> m) -> UArray a -> m
{-# INLINE foldMap' #-}
foldMap' f = foldr' (\x m -> f x <> m) mempty


-- -- Specialised folds
-- -- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.UArray as V
-- >>> V.all even $ V.fromList [2, 4, 12]
-- True
-- >>> V.all even $ V.fromList [2, 4, 13]
-- False
-- >>> V.all even (V.empty :: V.UArray Int)
-- True
all :: Unbox a => (a -> Bool) -> UArray a -> Bool
{-# INLINE all #-}
all f v = foldr (\x xs -> f x && xs) True v

-- | /O(n)/ Check if any element satisfies the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.UArray as V
-- >>> V.any even $ V.fromList [1, 3, 7]
-- False
-- >>> V.any even $ V.fromList [3, 2, 13]
-- True
-- >>> V.any even (V.empty :: V.UArray Int)
-- False
any :: Unbox a => (a -> Bool) -> UArray a -> Bool
{-# INLINE any #-}
any f = foldr (\x xs -> f x || xs) False

-- | /O(n)/ Check if all elements are 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.UArray as V
-- >>> V.and $ V.fromList [True, False]
-- False
-- >>> V.and V.empty
-- True
and :: UArray Bool -> Bool
{-# INLINE and #-}
and = foldr (&&) True

-- | /O(n)/ Check if any element is 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.UArray as V
-- >>> V.or $ V.fromList [True, False]
-- True
-- >>> V.or V.empty
-- False
or :: UArray Bool -> Bool
{-# INLINE or #-}
or = foldr (||) False

-- | /O(n)/ Compute the sum of the elements.
-- Warning: storing numbers (e.g. Int or Double) in a UArray
-- is inefficient because of redundant indirections.
-- Consider using unboxed UArrays (TODO) instead.
--
-- ==== __Examples__
--
-- >>> import qualified Data.UArray as V
-- >>> V.sum $ V.fromList [300,20,1]
-- 321
-- >>> V.sum (V.empty :: V.UArray Int)
-- 0
sum :: (Unbox a, Num a) => UArray a -> a
{-# INLINE sum #-}
sum = foldl' (+) 0

-- | /O(n)/ Compute the product of the elements.
-- Warning: storing numbers (e.g. Int or Double) in a UArray
-- is inefficient because of redundant indirections.
-- Consider using unboxed UArrays (TODO) instead.
--
-- ==== __Examples__
--
-- >>> import qualified Data.UArray as V
-- >>> V.product $ V.fromList [1,2,3,4]
-- 24
-- >>> V.product (V.empty :: V.UArray Int)
-- 1
product :: (Unbox a, Num a) => UArray a -> a
{-# INLINE product #-}
product = foldl' (*) 1

-- | /O(n)/ Yield the maximum element of the UArray. The UArray may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.UArray as V
-- >>> V.maximum $ V.fromList [2, 1]
-- 2
-- >>> import Data.Semigroup
-- >>> V.maximum $ V.fromList [Arg 1 'a', Arg 2 'b']
-- Arg 2 'b'
-- >>> V.maximum $ V.fromList [Arg 1 'a', Arg 1 'b']
-- Arg 1 'a'
maximum :: (Unbox a, Ord a) => UArray a -> a
{-# INLINE maximum #-}
maximum = foldl1' max

-- | /O(n)/ Yield the maximum element of the UArray according to the
-- given comparison function. The UArray may not be empty. In case of
-- a tie, the first occurrence wins. This behavior is different from
-- 'Data.List.maximumBy' which returns the last tie.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.UArray as V
-- >>> V.maximumBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- (2,'a')
-- >>> V.maximumBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
maximumBy :: Unbox a => (a -> a -> Ordering) -> UArray a -> a
{-# INLINE maximumBy #-}
maximumBy f = foldl1' (\x y -> case f x y of GT -> x; _ -> y)

-- | /O(n)/ Yield the maximum element of the UArray by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The UArray may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.UArray as V
-- >>> V.maximumOn fst $ V.fromList [(2,'a'), (1,'b')]
-- (2,'a')
-- >>> V.maximumOn fst $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
maximumOn :: (Unbox a, Ord b) => (a -> b) -> UArray a -> a
{-# INLINE maximumOn #-}
maximumOn f v = maybe (error "maximumOn: empty UArray") fst (foldl' (\s x ->
  case s of
    Just (!y, !fy) ->
      let !fx = f x in if fx > fy then Just (x, fx) else Just (y, fy)
    Nothing -> let !fx = f x in Just (x, fx)
  ) Nothing v)
  

-- | /O(n)/ Yield the minimum element of the UArray. The UArray may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.UArray as V
-- >>> V.minimum $ V.fromList [2, 1]
-- 1
-- >>> import Data.Semigroup
-- >>> V.minimum $ V.fromList [Arg 2 'a', Arg 1 'b']
-- Arg 1 'b'
-- >>> V.minimum $ V.fromList [Arg 1 'a', Arg 1 'b']
-- Arg 1 'a'
minimum :: (Unbox a, Ord a) => UArray a -> a
{-# INLINE minimum #-}
minimum = foldl1' min

-- | /O(n)/ Yield the minimum element of the UArray according to the
-- given comparison function. The UArray may not be empty. In case of
-- a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.UArray as V
-- >>> V.minimumBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- (1,'b')
-- >>> V.minimumBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
minimumBy :: Unbox a => (a -> a -> Ordering) -> UArray a -> a
{-# INLINE minimumBy #-}
minimumBy f = foldl1' (\x y -> case f x y of LT -> x; _ -> y)

-- | /O(n)/ Yield the minimum element of the UArray by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The UArray may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.UArray as V
-- >>> V.minimumOn fst $ V.fromList [(2,'a'), (1,'b')]
-- (1,'b')
-- >>> V.minimumOn fst $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
minimumOn :: (Unbox a, Ord b) => (a -> b) -> UArray a -> a
{-# INLINE minimumOn #-}
minimumOn f v = maybe (error "minimumOn: empty UArray") fst (foldl' (\s x ->
  case s of
    Just (!y, !fy) ->
      let !fx = f x in if fx < fy then Just (x, fx) else Just (y, fy)
    Nothing -> let !fx = f x in Just (x, fx)
  ) Nothing v)

-- -- | /O(n)/ Yield the index of the maximum element of the UArray. The UArray
-- -- may not be empty.
-- maxIndex :: Ord a => UArray a -> Int
-- {-# INLINE maxIndex #-}
-- maxIndex = G.maxIndex

-- -- | /O(n)/ Yield the index of the maximum element of the UArray
-- -- according to the given comparison function. The UArray may not be
-- -- empty. In case of a tie, the first occurrence wins.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import Data.Ord
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.maxIndexBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- -- 0
-- -- >>> V.maxIndexBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- -- 0
-- maxIndexBy :: (a -> a -> Ordering) -> UArray a -> Int
-- {-# INLINE maxIndexBy #-}
-- maxIndexBy = G.maxIndexBy

-- -- | /O(n)/ Yield the index of the minimum element of the UArray. The UArray
-- -- may not be empty.
-- minIndex :: Ord a => UArray a -> Int
-- {-# INLINE minIndex #-}
-- minIndex = G.minIndex

-- -- | /O(n)/ Yield the index of the minimum element of the UArray according to
-- -- the given comparison function. The UArray may not be empty.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import Data.Ord
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.minIndexBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- -- 1
-- -- >>> V.minIndexBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- -- 0
-- minIndexBy :: (a -> a -> Ordering) -> UArray a -> Int
-- {-# INLINE minIndexBy #-}
-- minIndexBy = G.minIndexBy

-- -- Monadic folds
-- -- -------------

-- | /O(n)/ Monadic fold.
foldM :: (Unbox b, Monad m) => (a -> b -> m a) -> a -> UArray b -> m a
{-# INLINE foldM #-}
-- TODO: this does not generate optimal Core/STG. i
-- I guess we'll need to implement these instead of the non-monadic folds.
foldM k z = foldl' (\m y -> do x <- m; k x y) (return z)

-- -- | /O(n)/ Monadic fold using a function applied to each element and its index.
-- ifoldM :: Monad m => (a -> Int -> b -> m a) -> a -> UArray b -> m a
-- {-# INLINE ifoldM #-}
-- ifoldM = G.ifoldM

-- -- | /O(n)/ Monadic fold over non-empty UArrays.
-- fold1M :: Monad m => (a -> a -> m a) -> UArray a -> m a
-- {-# INLINE fold1M #-}
-- fold1M = G.fold1M

-- -- | /O(n)/ Monadic fold with strict accumulator.
-- foldM' :: Monad m => (a -> b -> m a) -> a -> UArray b -> m a
-- {-# INLINE foldM' #-}
-- foldM' = G.foldM'

-- -- | /O(n)/ Monadic fold with strict accumulator using a function applied to each
-- -- element and its index.
-- ifoldM' :: Monad m => (a -> Int -> b -> m a) -> a -> UArray b -> m a
-- {-# INLINE ifoldM' #-}
-- ifoldM' = G.ifoldM'

-- -- | /O(n)/ Monadic fold over non-empty UArrays with strict accumulator.
-- fold1M' :: Monad m => (a -> a -> m a) -> UArray a -> m a
-- {-# INLINE fold1M' #-}
-- fold1M' = G.fold1M'

-- -- | /O(n)/ Monadic fold that discards the result.
-- foldM_ :: Monad m => (a -> b -> m a) -> a -> UArray b -> m ()
-- {-# INLINE foldM_ #-}
-- foldM_ = G.foldM_

-- -- | /O(n)/ Monadic fold that discards the result using a function applied to
-- -- each element and its index.
-- ifoldM_ :: Monad m => (a -> Int -> b -> m a) -> a -> UArray b -> m ()
-- {-# INLINE ifoldM_ #-}
-- ifoldM_ = G.ifoldM_

-- -- | /O(n)/ Monadic fold over non-empty UArrays that discards the result.
-- fold1M_ :: Monad m => (a -> a -> m a) -> UArray a -> m ()
-- {-# INLINE fold1M_ #-}
-- fold1M_ = G.fold1M_

-- -- | /O(n)/ Monadic fold with strict accumulator that discards the result.
-- foldM'_ :: Monad m => (a -> b -> m a) -> a -> UArray b -> m ()
-- {-# INLINE foldM'_ #-}
-- foldM'_ = G.foldM'_

-- -- | /O(n)/ Monadic fold with strict accumulator that discards the result
-- -- using a function applied to each element and its index.
-- ifoldM'_ :: Monad m => (a -> Int -> b -> m a) -> a -> UArray b -> m ()
-- {-# INLINE ifoldM'_ #-}
-- ifoldM'_ = G.ifoldM'_

-- -- | /O(n)/ Monadic fold over non-empty UArrays with strict accumulator
-- -- that discards the result.
-- fold1M'_ :: Monad m => (a -> a -> m a) -> UArray a -> m ()
-- {-# INLINE fold1M'_ #-}
-- fold1M'_ = G.fold1M'_

-- -- Monadic sequencing
-- -- ------------------

-- -- -- | Evaluate each action and collect the results.
-- sequence :: Monad m => UArray (m a) -> m (UArray a)
-- {-# INLINE sequence #-}
-- sequence v = generateM (length v) (\i -> unsafeIndex v i)
-- {-# SPECIALIZE sequence :: UArray (IO a) -> IO (UArray a) #-}

-- -- | Evaluate each action and discard the results.
-- sequence_ :: Monad m => UArray (m a) -> m ()
-- {-# INLINE sequence_ #-}
-- sequence_ = foldr (\m xs -> m >> xs) (return ())
-- {-# SPECIALIZE sequence_ :: UArray (IO a) -> IO () #-}

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
-- >>> import qualified Data.UArray as V
-- >>> V.prescanl (+) 0 (V.fromList [1,2,3,4])
-- [0,1,3,6]
prescanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> UArray b -> UArray a
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
-- >>> import qualified Data.UArray as V
-- >>> V.postscanl (+) 0 (V.fromList [1,2,3,4])
-- [1,3,6,10]
postscanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> UArray b -> UArray a
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
-- >>> import qualified Data.UArray as V
-- >>> V.scanl (+) 0 (V.fromList [1,2,3,4])
-- [0,1,3,6,10]
scanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> UArray b -> UArray a
{-# INLINE scanl #-}
scanl k z v = iscanl (\_ -> k) z v

-- | /O(n)/ Left-to-right scan over a UArray (strictly) with its index.
iscanl :: (Unbox a, Unbox b) => (Int -> a -> b -> a) -> a -> UArray b -> UArray a
{-# INLINE iscanl #-}
iscanl k z v = iunfoldrExactN (length v + 1) (\i s -> if i == length v then (s,s) else let !x = unsafeIndex v i; s' = k i s x in (s, s')) z

-- | /O(n)/ Initial-value free left-to-right scan over a UArray with a strict accumulator.
--
-- ==== __Examples__
-- >>> import qualified Data.UArray as V
-- >>> V.scanl1 min $ V.fromListN 5 [4,2,4,1,3]
-- [4,2,2,1,1]
-- >>> V.scanl1 max $ V.fromListN 5 [1,3,2,5,4]
-- [1,3,3,5,5]
-- >>> V.scanl1 min (V.empty :: V.UArray Int)
-- []
scanl1 :: Unbox a => (a -> a -> a) -> UArray a -> UArray a
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
-- prescanr :: (a -> b -> b) -> b -> UArray a -> UArray b
-- {-# INLINE prescanr #-}
-- prescanr = G.prescanr

-- -- | /O(n)/ Right-to-left prescan with strict accumulator.
-- prescanr' :: (a -> b -> b) -> b -> UArray a -> UArray b
-- {-# INLINE prescanr' #-}
-- prescanr' = G.prescanr'

-- -- | /O(n)/ Right-to-left postscan.
-- postscanr :: (a -> b -> b) -> b -> UArray a -> UArray b
-- {-# INLINE postscanr #-}
-- postscanr = G.postscanr

-- -- | /O(n)/ Right-to-left postscan with strict accumulator.
-- postscanr' :: (a -> b -> b) -> b -> UArray a -> UArray b
-- {-# INLINE postscanr' #-}
-- postscanr' = G.postscanr'

-- -- | /O(n)/ Right-to-left scan.
-- scanr :: (a -> b -> b) -> b -> UArray a -> UArray b
-- {-# INLINE scanr #-}
-- scanr = G.scanr

-- -- | /O(n)/ Right-to-left scan with strict accumulator.
-- scanr' :: (a -> b -> b) -> b -> UArray a -> UArray b
-- {-# INLINE scanr' #-}
-- scanr' = G.scanr'

-- -- | /O(n)/ Right-to-left scan over a UArray with its index.
-- --
-- -- @since 0.12.0.0
-- iscanr :: (Int -> a -> b -> b) -> b -> UArray a -> UArray b
-- {-# INLINE iscanr #-}
-- iscanr = G.iscanr

-- -- | /O(n)/ Right-to-left scan over a UArray (strictly) with its index.
-- --
-- -- @since 0.12.0.0
-- iscanr' :: (Int -> a -> b -> b) -> b -> UArray a -> UArray b
-- {-# INLINE iscanr' #-}
-- iscanr' = G.iscanr'

-- -- | /O(n)/ Right-to-left, initial-value free scan over a UArray.
-- --
-- -- Note: Since 0.13, application of this to an empty UArray no longer
-- -- results in an error; instead it produces an empty UArray.
-- --
-- -- ==== __Examples__
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.scanr1 min $ V.fromListN 5 [3,1,4,2,4]
-- -- [1,1,2,2,4]
-- -- >>> V.scanr1 max $ V.fromListN 5 [4,5,2,3,1]
-- -- [5,5,3,3,1]
-- -- >>> V.scanr1 min (V.empty :: V.UArray Int)
-- -- []
-- scanr1 :: (a -> a -> a) -> UArray a -> UArray a
-- {-# INLINE scanr1 #-}
-- scanr1 = G.scanr1

-- -- | /O(n)/ Right-to-left, initial-value free scan over a UArray with a strict
-- -- accumulator.
-- --
-- -- Note: Since 0.13, application of this to an empty UArray no longer
-- -- results in an error; instead it produces an empty UArray.
-- --
-- -- ==== __Examples__
-- -- >>> import qualified Data.UArray as V
-- -- >>> V.scanr1' min $ V.fromListN 5 [3,1,4,2,4]
-- -- [1,1,2,2,4]
-- -- >>> V.scanr1' max $ V.fromListN 5 [4,5,2,3,1]
-- -- [5,5,3,3,1]
-- -- >>> V.scanr1' min (V.empty :: V.UArray Int)
-- -- []
-- scanr1' :: (a -> a -> a) -> UArray a -> UArray a
-- {-# INLINE scanr1' #-}
-- scanr1' = G.scanr1'

-- -- Comparisons
-- -- ------------------------

-- | /O(n)/ Check if two UArrays are equal using the supplied equality
-- predicate.
eqBy :: (Unbox a, Unbox b) => (a -> b -> Bool) -> UArray a -> UArray b -> Bool
{-# INLINE eqBy #-}
eqBy eq v w = ifoldr (\i x xs -> let !y = unsafeIndex w i in eq x y && xs) True v

-- | /O(n)/ Compare two UArrays using the supplied comparison function for
-- UArray elements. Comparison works the same as for lists (lexicographically).
--
-- > cmpBy compare == compare
cmpBy :: (Unbox a, Unbox b) => (a -> b -> Ordering) -> UArray a -> UArray b -> Ordering
{-# INLINE cmpBy #-}
cmpBy cmp v w = ifoldr (\i x xs -> let !y = unsafeIndex w i in cmp x y <> xs) EQ v

-- -- Conversions - Lists
-- -- ------------------------

-- | /O(n)/ Convert a UArray to a list. Can fuse!
toList :: Unbox a => UArray a -> [a]
{-# INLINE toList #-}
toList v = GHC.build (\c n ->
  let 
    go i
      | i < length v = let !x = unsafeIndex v i in x `c` go (i + 1)
      | otherwise = n
  in go 0)

-- | /O(n)/ Convert a list to a UArray. During the operation, the 
-- UArray’s capacity will be doubling until the list's contents are 
-- in the UArray.
fromList :: Unbox a => [a] -> UArray a
{-# INLINE fromList #-}
fromList xs = runST (do
  m <- G.new
  Prelude.mapM_ (G.pushBack m) xs
  unsafePetrify m)

-- | /O(n)/ Convert the first @n@ elements of a list to a UArray. It's
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
fromListN :: Unbox a => Int -> [a] -> UArray a
{-# INLINE fromListN #-}
fromListN n xs = runST (do
  m <- M.new n
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

-- | /O(1)/ Unsafely convert a mutable UArray to an immutable one without
-- copying. The mutable UArray may not be used after this operation.
unsafeFreeze :: M.STUArray s a -> ST s (UArray a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze (M.UnsafeSTUArray (MutableByteArray marr)) = GHC.ST (\s ->
  case GHC.unsafeFreezeByteArray# marr s of
    (# s', arr #) -> (# s', UnsafeUArray (ByteArray arr) #))

-- -- | A slice (subUArray) of an immutable UArray. This takes up 2 extra words, so /4 + n/ words total.
-- data UArraySlice a = UnsafeUArraySlice {-# UNPACK #-} !Int !Int !(UArray a) 

-- -- | Convert a UArray to a slice which covers the whole UArray.
-- whole :: UArray a -> UArraySlice a
-- whole v = UnsafeUArraySlice 0 (length v) v

-- -- | Take a prefix of a slice
-- unsafeTakeL :: Int -> UArraySlice a -> UArraySlice a
-- unsafeTakeL n (UnsafeUArraySlice off _ m) = UnsafeUArraySlice off n m

-- -- | Take a suffix of a slice
-- unsafeTakeR :: Int -> UArraySlice a -> UArraySlice a
-- unsafeTakeR n (UnsafeUArraySlice off len m) = UnsafeUArraySlice (off + len - n) n m

-- -- | Remove a prefix of a slice
-- unsafeDropL :: Int -> UArraySlice a -> UArraySlice a
-- unsafeDropL n (UnsafeUArraySlice off len m) = UnsafeUArraySlice (off + n) (len - n) m

-- -- | Remove a suffix of a slice
-- unsafeDropR :: Int -> UArraySlice a -> UArraySlice a
-- unsafeDropR n (UnsafeUArraySlice off len m) = UnsafeUArraySlice off (len - n) m

-- -- | /O(n)/ Yield an immutable copy of the mutable UArray.
-- freeze :: M.STUArraySlice s a -> ST s (UArray a)
-- {-# INLINE freeze #-}
-- freeze (M.UnsafeSTUArraySlice (M.UnsafeSTUArray marr) (GHC.I# off) (GHC.I# len)) = GHC.ST (\s ->
--   case GHC.freezeSmallUArray# marr off len s of
--     (# s', arr #) -> (# s', UnsafeUArray arr #))

-- -- | /O(n)/ Yield a mutable copy of an immutable UArray.
-- thaw :: UArraySlice a -> ST s (M.STUArray s a)
-- {-# INLINE thaw #-}
-- thaw (UnsafeUArraySlice (GHC.I# i) (GHC.I# n) (UnsafeUArray arr)) = GHC.ST (\s -> 
--   case GHC.thawSmallUArray# arr i n s of
--     (# s', marr #) -> (# s', M.UnsafeSTUArray marr #))

-- -- | /O(n)/ Copy an immutable UArray into a mutable one.
-- unsafeCopy :: UArraySlice a -> M.STUArraySlice s a -> ST s ()
-- {-# INLINE unsafeCopy #-}
-- unsafeCopy (UnsafeUArraySlice (GHC.I# offv) _ (UnsafeUArray arr)) (M.UnsafeSTUArraySlice (M.UnsafeSTUArray marr) (GHC.I# offm) (GHC.I# len)) = GHC.ST (\s -> 
--   (# GHC.copySmallUArray# arr offv marr offm len s , () #))

-- -- | /O(n)/ Copy an immutable UArray into a mutable one. The two UArrays must
-- -- have the same length.
-- copy :: UArraySlice a -> M.STUArraySlice s a -> ST s ()
-- {-# INLINE copy #-}
-- copy v@(UnsafeUArraySlice _ vn _) m@(M.UnsafeSTUArraySlice _ mn _)
--   | mn == vn = unsafeCopy v m
--   | otherwise = error "copy: UArray slices have different lengths"

-- -- | /O(n)/ Yield an immutable copy of a grow UArray.
-- petrify :: G.GrowUArray s a -> ST s (UArray a)
-- petrify (G.UnsafeGrowUArray ref) = do
--   G.UnsafeGrowUArray_ n m <- readSTRef ref
--   freeze (M.unsafeTakeL n (M.whole m))

-- | /O(1)/ Convert a grow UArray to an immutable UArray. The grow UArray must
-- not be used after this.
unsafePetrify :: Unbox a => G.GrowUArray s a -> ST s (UArray a)
unsafePetrify (G.UnsafeGrowUArray ref) = do
  G.UnsafeGrowUArray_ n m <- readSTRef ref
  M.unsafeShrink m n
  unsafeFreeze m
  
-- $setup
-- >>> :set -Wno-type-defaults
-- >>> import Prelude (Char, String, Bool(True, False), min, max, fst, even, undefined, Ord(..), ($), (<>), Num(..))
