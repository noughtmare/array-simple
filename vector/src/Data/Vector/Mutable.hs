{-# LANGUAGE CPP #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- |
-- Module      : Data.Vector.Mutable
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
-- Mutable boxed vectors.

module Data.Vector.Mutable (
  -- * Mutable boxed vectors
  -- STVector(STVector), IOVector, 
  STVector (..),

  -- -- * Accessors

  -- -- ** Length information
  length, null,

  -- -- ** Extracting subvectors
  -- slice, init, tail, take, drop, splitAt,
  -- unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- -- ** Overlapping
  -- overlaps,

  -- -- * Construction

  -- -- ** Initialisation
  new, unsafeNew, 
  -- replicate, replicateM, generate, generateM, 
  clone,

  -- -- ** Growing
  -- grow, unsafeGrow,

  -- -- ** Restricting memory usage
  -- clear,

  -- -- * Accessing individual elements
  read, readMaybe, write, 
  -- modify, modifyM, swap, exchange,
  unsafeRead, unsafeWrite, 
  -- unsafeModify, unsafeModifyM, unsafeSwap, unsafeExchange,

  -- -- * Folds
  -- mapM_, imapM_, forM_, iforM_,
  -- foldl, foldl', foldM, foldM',
  -- foldr, foldr', foldrM, foldrM',
  -- ifoldl, ifoldl', ifoldM, ifoldM',
  -- ifoldr, ifoldr', ifoldrM, ifoldrM',

  -- -- * Modifying vectors
  -- mapInPlace, imapInPlace, mapInPlaceM, imapInPlaceM,
  -- nextPermutation, nextPermutationBy,
  -- prevPermutation, prevPermutationBy,

  -- -- ** Filling and copying
  -- set, copy, move, unsafeCopy, unsafeMove,

  -- -- ** Arrays
  -- fromMutableArray, toMutableArray,

  -- ** Slice
  STVectorSlice (..), whole, unsafeTakeL, unsafeTakeR, unsafeDropL, unsafeDropR,

  -- -- * Re-exports
  -- PrimMonad, PrimState, RealWorld
) where

-- import qualified Data.Vector.Generic.Mutable as G
-- import           Data.Vector.Mutable.Unsafe (STVector(..))
-- import           Data.Vector.Pattern
-- import           Data.Primitive.Array
-- import           Control.Monad.Primitive
import Data.Elevator
import qualified GHC.Exts as GHC
import qualified GHC.ST as GHC
import Control.Monad.ST

import Prelude( Eq (..), Ord (..), Bool, Ordering(..), Int, Maybe, (<$>), error, otherwise, (&&), (||), pure, Maybe (..), Num (..))

data STVector s a = UnsafeSTVector (GHC.MutableArray# s (Strict a))




-- #include "vector.h"

-- type IOVector = STVector RealWorld
-- type STVector s = STVector s


-- -- Conversions - Arrays
-- -- -----------------------------

-- -- | /O(n)/ Make a copy of a mutable array to a new mutable vector.
-- --
-- -- @since 0.12.2.0
-- fromMutableArray :: MutableArray s a -> ST s (STVector s a)
-- {-# INLINE fromMutableArray #-}
-- fromMutableArray marr =
--   let size = sizeofMutableArray marr
--   in UnsafeSTVector 0 size <$> cloneMutableArray marr 0 size

-- -- | /O(n)/ Make a copy of a mutable vector into a new mutable array.
-- --
-- -- @since 0.12.2.0
-- toMutableArray :: STVector s a -> ST s (MutableArray s a)
-- {-# INLINE toMutableArray #-}
-- toMutableArray (UnsafeSTVector offset size marr) = cloneMutableArray marr offset size


-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: STVector s a -> Int
{-# INLINE length #-}
length (UnsafeSTVector m) = GHC.I# (GHC.sizeofMutableArray# m)

-- | Check whether the vector is empty.
null :: STVector s a -> Bool
{-# INLINE null #-}
null m = length m == 0

-- -- Extracting subvectors
-- -- ---------------------

-- -- | Yield a part of the mutable vector without copying it. The vector must
-- -- contain at least @i+n@ elements.
-- slice :: Int  -- ^ @i@ starting index
--       -> Int  -- ^ @n@ length
--       -> STVector s a
--       -> STVector s a
-- {-# INLINE slice #-}
-- slice = G.slice

-- -- | Take the @n@ first elements of the mutable vector without making a
-- -- copy. For negative @n@, the empty vector is returned. If @n@ is larger
-- -- than the vector's length, the vector is returned unchanged.
-- take :: Int -> STVector s a -> STVector s a
-- {-# INLINE take #-}
-- take = G.take

-- -- | Drop the @n@ first element of the mutable vector without making a
-- -- copy. For negative @n@, the vector is returned unchanged. If @n@ is
-- -- larger than the vector's length, the empty vector is returned.
-- drop :: Int -> STVector s a -> STVector s a
-- {-# INLINE drop #-}
-- drop = G.drop

-- -- | /O(1)/ Split the mutable vector into the first @n@ elements
-- -- and the remainder, without copying.
-- --
-- -- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@,
-- -- but slightly more efficient.
-- splitAt :: Int -> STVector s a -> (STVector s a, STVector s a)
-- {-# INLINE splitAt #-}
-- splitAt = G.splitAt

-- -- | Drop the last element of the mutable vector without making a copy.
-- -- If the vector is empty, an exception is thrown.
-- init :: STVector s a -> STVector s a
-- {-# INLINE init #-}
-- init = G.init

-- -- | Drop the first element of the mutable vector without making a copy.
-- -- If the vector is empty, an exception is thrown.
-- tail :: STVector s a -> STVector s a
-- {-# INLINE tail #-}
-- tail = G.tail

-- -- | Yield a part of the mutable vector without copying it. No bounds checks
-- -- are performed.
-- unsafeSlice :: Int  -- ^ starting index
--             -> Int  -- ^ length of the slice
--             -> STVector s a
--             -> STVector s a
-- {-# INLINE unsafeSlice #-}
-- unsafeSlice = G.unsafeSlice

-- -- | Unsafe variant of 'take'. If @n@ is out of range, it will
-- -- simply create an invalid slice that likely violate memory safety.
-- unsafeTake :: Int -> STVector s a -> STVector s a
-- {-# INLINE unsafeTake #-}
-- unsafeTake = G.unsafeTake

-- -- | Unsafe variant of 'drop'. If @n@ is out of range, it will
-- -- simply create an invalid slice that likely violate memory safety.
-- unsafeDrop :: Int -> STVector s a -> STVector s a
-- {-# INLINE unsafeDrop #-}
-- unsafeDrop = G.unsafeDrop

-- -- | Same as 'init', but doesn't do range checks.
-- unsafeInit :: STVector s a -> STVector s a
-- {-# INLINE unsafeInit #-}
-- unsafeInit = G.unsafeInit

-- -- | Same as 'tail', but doesn't do range checks.
-- unsafeTail :: STVector s a -> STVector s a
-- {-# INLINE unsafeTail #-}
-- unsafeTail = G.unsafeTail

-- -- Overlapping
-- -- -----------

-- -- | Check whether two vectors overlap.
-- overlaps :: STVector s a -> STVector s a -> Bool
-- {-# INLINE overlaps #-}
-- overlaps = G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: Int -> a -> ST s (STVector s a)
{-# INLINE new #-}
new (GHC.I# n) x = GHC.ST (\s -> 
  case GHC.newArray# n (Strict x) s of { (# s', marr #) ->
    (# s', UnsafeSTVector marr #)
  })

-- | Create a mutable vector of the given length. The vector elements
-- are set to an undefined value, so accessing them will cause a segfault at best.
--
-- @since 0.5
unsafeNew :: Int -> ST s (STVector s a)
{-# INLINE unsafeNew #-}
unsafeNew (GHC.I# n) = GHC.ST (\s ->
  case GHC.newArray# n (GHC.unsafeCoerce# ()) s of { (# s', marr #) ->
    (# s', UnsafeSTVector marr #)
  })

-- -- | Create a mutable vector of the given length (0 if the length is negative)
-- -- and fill it with an initial value.
-- replicate :: Int -> a -> ST s (STVector s a)
-- {-# INLINE replicate #-}
-- replicate = G.replicate

-- -- | Create a mutable vector of the given length (0 if the length is negative)
-- -- and fill it with values produced by repeatedly executing the monadic action.
-- replicateM :: Int -> ST s a -> ST s (STVector s a)
-- {-# INLINE replicateM #-}
-- replicateM = G.replicateM

-- -- | /O(n)/ Create a mutable vector of the given length (0 if the length is negative)
-- -- and fill it with the results of applying the function to each index.
-- -- Iteration starts at index 0.
-- --
-- -- @since 0.12.3.0
-- generate :: Int -> (Int -> a) -> ST s (STVector s a)
-- {-# INLINE generate #-}
-- generate = G.generate

-- -- | /O(n)/ Create a mutable vector of the given length (0 if the length is
-- -- negative) and fill it with the results of applying the monadic function to each
-- -- index. Iteration starts at index 0.
-- --
-- -- @since 0.12.3.0
-- generateM :: Int -> (Int -> ST s a) -> ST s (STVector s a)
-- {-# INLINE generateM #-}
-- generateM = G.generateM

-- | Create a copy of a mutable vector.
clone :: STVector s a -> ST s (STVector s a)
{-# INLINE clone #-}
clone (UnsafeSTVector marr) = GHC.ST (\s -> 
  case GHC.cloneMutableArray# marr 0# (GHC.sizeofMutableArray# marr) s of
    (# s', marr' #) -> (# s', UnsafeSTVector marr' #))


-- -- Growing
-- -- -------

-- -- | Grow a boxed vector by the given number of elements. The number must be
-- -- non-negative. This has the same semantics as 'G.grow' for generic vectors. It differs
-- -- from @grow@ functions for unpacked vectors, however, in that only pointers to
-- -- values are copied over, therefore the values themselves will be shared between the
-- -- two vectors. This is an important distinction to know about during memory
-- -- usage analysis and in case the values themselves are of a mutable type, e.g.
-- -- 'Data.IORef.IORef' or another mutable vector.
-- --
-- -- ==== __Examples__
-- --
-- -- >>> import qualified Data.Vector as V
-- -- >>> import qualified Data.Vector.Mutable as MV
-- -- >>> mv <- V.thaw $ V.fromList ([10, 20, 30] :: [Integer])
-- -- >>> mv' <- MV.grow mv 2
-- --
-- -- The two extra elements at the end of the newly allocated vector will be
-- -- uninitialized and will result in an error if evaluated, so me must overwrite
-- -- them with new values first:
-- --
-- -- >>> MV.write mv' 3 999
-- -- >>> MV.write mv' 4 777
-- -- >>> V.freeze mv'
-- -- [10,20,30,999,777]
-- --
-- -- It is important to note that the source mutable vector is not affected when
-- -- the newly allocated one is mutated.
-- --
-- -- >>> MV.write mv' 2 888
-- -- >>> V.freeze mv'
-- -- [10,20,888,999,777]
-- -- >>> V.freeze mv
-- -- [10,20,30]
-- --
-- -- @since 0.5
-- grow :: PrimMonad m
--      => STVector s a -> Int -> ST s (STVector s a)
-- {-# INLINE grow #-}
-- grow = G.grow

-- -- | Grow a vector by the given number of elements. The number must be non-negative, but
-- -- this is not checked. This has the same semantics as 'G.unsafeGrow' for generic vectors.
-- --
-- -- @since 0.5
-- unsafeGrow :: PrimMonad m
--            => STVector s a -> Int -> ST s (STVector s a)
-- {-# INLINE unsafeGrow #-}
-- unsafeGrow = G.unsafeGrow

-- -- Restricting memory usage
-- -- ------------------------

-- -- | Reset all elements of the vector to some undefined value, clearing all
-- -- references to external objects.
-- clear :: STVector s a -> ST s ()
-- {-# INLINE clear #-}
-- clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position. Will throw an exception if
-- the index is out of range.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Mutable as MV
-- >>> v <- MV.generate 10 (\x -> x*x)
-- >>> MV.read v 3
-- 9
read :: STVector s a -> Int -> ST s a
{-# INLINE read #-}
read m i | 0 <= i && i < length m = unsafeRead m i
         | otherwise = error "read: index out of bounds"

-- | Yield the element at the given position. Returns 'Nothing' if
-- the index is out of range.
--
-- @since 0.13
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Mutable as MV
-- >>> v <- MV.generate 10 (\x -> x*x)
-- >>> MV.readMaybe v 3
-- Just 9
-- >>> MV.readMaybe v 13
-- Nothing
readMaybe :: STVector s a -> Int -> ST s (Maybe a)
{-# INLINE readMaybe #-}
readMaybe m i 
  | 0 <= i && i < length m = Just <$> unsafeRead m i
  | otherwise = pure Nothing

-- | Replace the element at the given position.
write :: STVector s a -> Int -> a -> ST s ()
{-# INLINE write #-}
write m i x
  | 0 <= i && i < length m = unsafeWrite m i x
  | otherwise = error "write: index out of bounds"

-- -- | Modify the element at the given position.
-- modify :: STVector s a -> (a -> a) -> Int -> ST s ()
-- {-# INLINE modify #-}
-- modify m f i
--   | 0 <= i && i < length m = unsafeModify m f i
--   | otherwise = error "modify: index out of bounds"

-- -- | Modify the element at the given position using a monadic function.
-- --
-- -- @since 0.12.3.0
-- modifyM :: STVector s a -> (a -> ST s a) -> Int -> ST s ()
-- {-# INLINE modifyM #-}
-- modifyM = G.modifyM

-- -- | Swap the elements at the given positions.
-- swap :: STVector s a -> Int -> Int -> ST s ()
-- {-# INLINE swap #-}
-- swap = G.swap

-- -- | Replace the element at the given position and return the old element.
-- exchange :: STVector s a -> Int -> a -> ST s a
-- {-# INLINE exchange #-}
-- exchange = G.exchange

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: STVector s a -> Int -> ST s a
{-# INLINE unsafeRead #-}
unsafeRead (UnsafeSTVector m) (GHC.I# i) = GHC.ST (\s -> 
  case GHC.readArray# m i s of
    (# s', Strict x #) -> (# s', x #))

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: STVector s a -> Int -> a -> ST s ()
{-# INLINE unsafeWrite #-}
unsafeWrite (UnsafeSTVector m) (GHC.I# i) x = GHC.ST (\s ->
  (# GHC.writeArray# m i (Strict x) s , () #))

-- -- | Modify the element at the given position. No bounds checks are performed.
-- unsafeModify :: STVector s a -> (a -> a) -> Int -> ST s ()
-- {-# INLINE unsafeModify #-}
-- unsafeModify m f i = do
--   x <- unsafeRead m i
--   unsafeWrite m i (f x)

-- -- | Modify the element at the given position using a monadic
-- -- function. No bounds checks are performed.
-- --
-- -- @since 0.12.3.0
-- unsafeModifyM :: STVector s a -> (a -> ST s a) -> Int -> ST s ()
-- {-# INLINE unsafeModifyM #-}
-- unsafeModifyM = G.unsafeModifyM

-- -- | Swap the elements at the given positions. No bounds checks are performed.
-- unsafeSwap :: STVector s a -> Int -> Int -> ST s ()
-- {-# INLINE unsafeSwap #-}
-- unsafeSwap = G.unsafeSwap

-- -- | Replace the element at the given position and return the old element. No
-- -- bounds checks are performed.
-- unsafeExchange :: STVector s a -> Int -> a -> ST s a
-- {-# INLINE unsafeExchange #-}
-- unsafeExchange = G.unsafeExchange

-- -- Filling and copying
-- -- -------------------

-- -- | Set all elements of the vector to the given value.
-- set :: STVector s a -> a -> ST s ()
-- {-# INLINE set #-}
-- set = G.set

-- -- | Copy a vector. The two vectors must have the same length and may not
-- -- overlap.
-- copy :: STVector s a   -- ^ target
--                     -> STVector s a   -- ^ source
--                     -> ST s ()
-- {-# INLINE copy #-}
-- copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap, but this is not checked.
-- unsafeCopy :: STVector s a   -- ^ target
--            -> STVector s a   -- ^ source
--            -> ST s ()
-- {-# INLINE unsafeCopy #-}
-- unsafeCopy (UnsafeSTVector marr') (UnsafeSTVector marr) = GHC.ST (\s ->
--   case 
--   _)

-- -- | Move the contents of a vector. The two vectors must have the same
-- -- length.
-- --
-- -- If the vectors do not overlap, then this is equivalent to 'copy'.
-- -- Otherwise, the copying is performed as if the source vector were
-- -- copied to a temporary vector and then the temporary vector was copied
-- -- to the target vector.
-- move :: STVector s a   -- ^ target
--                     -> STVector s a   -- ^ source
--                     -> ST s ()
-- {-# INLINE move #-}
-- move = G.move

-- -- | Move the contents of a vector. The two vectors must have the same
-- -- length, but this is not checked.
-- --
-- -- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- -- Otherwise, the copying is performed as if the source vector were
-- -- copied to a temporary vector and then the temporary vector was copied
-- -- to the target vector.
-- unsafeMove :: STVector s a   -- ^ target
--                           -> STVector s a   -- ^ source
--                           -> ST s ()
-- {-# INLINE unsafeMove #-}
-- unsafeMove = G.unsafeMove

-- -- Modifying vectors
-- -- -----------------

-- -- | Modify vector in place by applying function to each element.
-- --
-- -- @since NEXT_VERSION
-- mapInPlace :: (a -> a) -> STVector s a -> ST s ()
-- {-# INLINE mapInPlace #-}
-- mapInPlace = G.mapInPlace

-- -- | Modify vector in place by applying function to each element and its index.
-- --
-- -- @since NEXT_VERSION
-- imapInPlace :: (Int -> a -> a) -> STVector s a -> ST s ()
-- {-# INLINE imapInPlace #-}
-- imapInPlace = G.imapInPlace

-- -- | Modify vector in place by applying monadic function to each element in order.
-- --
-- -- @since NEXT_VERSION
-- mapInPlaceM :: (a -> ST s a) -> STVector s a -> ST s ()
-- {-# INLINE mapInPlaceM #-}
-- mapInPlaceM = G.mapInPlaceM

-- -- | Modify vector in place by applying monadic function to each element and its index in order.
-- --
-- -- @since NEXT_VERSION
-- imapInPlaceM :: (Int -> a -> ST s a) -> STVector s a -> ST s ()
-- {-# INLINE imapInPlaceM #-}
-- imapInPlaceM = G.imapInPlaceM

-- -- | Compute the (lexicographically) next permutation of the given vector in-place.
-- -- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- -- weakly descending order. In this case the vector will not get updated,
-- -- as opposed to the behavior of the C++ function @std::next_permutation@.
-- nextPermutation :: (PrimMonad m, Ord e) => STVector s e -> m Bool
-- {-# INLINE nextPermutation #-}
-- nextPermutation = G.nextPermutation

-- -- | Compute the (lexicographically) next permutation of the given vector in-place,
-- -- using the provided comparison function.
-- -- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- -- weakly descending order. In this case the vector will not get updated,
-- -- as opposed to the behavior of the C++ function @std::next_permutation@.
-- --
-- -- @since 0.13.2.0
-- nextPermutationBy :: (e -> e -> Ordering) -> STVector s e -> m Bool
-- {-# INLINE nextPermutationBy #-}
-- nextPermutationBy = G.nextPermutationBy

-- -- | Compute the (lexicographically) previous permutation of the given vector in-place.
-- -- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- -- weakly ascending order. In this case the vector will not get updated,
-- -- as opposed to the behavior of the C++ function @std::prev_permutation@.
-- --
-- -- @since 0.13.2.0
-- prevPermutation :: (PrimMonad m, Ord e) => STVector s e -> m Bool
-- {-# INLINE prevPermutation #-}
-- prevPermutation = G.prevPermutation

-- -- | Compute the (lexicographically) previous permutation of the given vector in-place,
-- -- using the provided comparison function.
-- -- Returns False when the input is the last item in the enumeration, i.e., if it is in
-- -- weakly ascending order. In this case the vector will not get updated,
-- -- as opposed to the behavior of the C++ function @std::prev_permutation@.
-- --
-- -- @since 0.13.2.0
-- prevPermutationBy :: (e -> e -> Ordering) -> STVector s e -> m Bool
-- {-# INLINE prevPermutationBy #-}
-- prevPermutationBy = G.prevPermutationBy

-- -- Folds
-- -- -----

-- -- | /O(n)/ Apply the monadic action to every element of the vector, discarding the results.
-- --
-- -- @since 0.12.3.0
-- mapM_ :: (a -> m b) -> STVector s a -> ST s ()
-- {-# INLINE mapM_ #-}
-- mapM_ = G.mapM_

-- -- | /O(n)/ Apply the monadic action to every element of the vector and its index, discarding the results.
-- --
-- -- @since 0.12.3.0
-- imapM_ :: (Int -> a -> m b) -> STVector s a -> ST s ()
-- {-# INLINE imapM_ #-}
-- imapM_ = G.imapM_

-- -- | /O(n)/ Apply the monadic action to every element of the vector,
-- -- discarding the results. It's the same as @flip mapM_@.
-- --
-- -- @since 0.12.3.0
-- forM_ :: STVector s a -> (a -> m b) -> ST s ()
-- {-# INLINE forM_ #-}
-- forM_ = G.forM_

-- -- | /O(n)/ Apply the monadic action to every element of the vector
-- -- and its index, discarding the results. It's the same as @flip imapM_@.
-- --
-- -- @since 0.12.3.0
-- iforM_ :: STVector s a -> (Int -> a -> m b) -> ST s ()
-- {-# INLINE iforM_ #-}
-- iforM_ = G.iforM_

-- -- | /O(n)/ Pure left fold.
-- --
-- -- @since 0.12.3.0
-- foldl :: (b -> a -> b) -> b -> STVector s a -> m b
-- {-# INLINE foldl #-}
-- foldl = G.foldl

-- -- | /O(n)/ Pure left fold with strict accumulator.
-- --
-- -- @since 0.12.3.0
-- foldl' :: (b -> a -> b) -> b -> STVector s a -> m b
-- {-# INLINE foldl' #-}
-- foldl' = G.foldl'

-- -- | /O(n)/ Pure left fold using a function applied to each element and its index.
-- --
-- -- @since 0.12.3.0
-- ifoldl :: (b -> Int -> a -> b) -> b -> STVector s a -> m b
-- {-# INLINE ifoldl #-}
-- ifoldl = G.ifoldl

-- -- | /O(n)/ Pure left fold with strict accumulator using a function applied to each element and its index.
-- --
-- -- @since 0.12.3.0
-- ifoldl' :: (b -> Int -> a -> b) -> b -> STVector s a -> m b
-- {-# INLINE ifoldl' #-}
-- ifoldl' = G.ifoldl'

-- -- | /O(n)/ Pure right fold.
-- --
-- -- @since 0.12.3.0
-- foldr :: (a -> b -> b) -> b -> STVector s a -> m b
-- {-# INLINE foldr #-}
-- foldr = G.foldr

-- -- | /O(n)/ Pure right fold with strict accumulator.
-- --
-- -- @since 0.12.3.0
-- foldr' :: (a -> b -> b) -> b -> STVector s a -> m b
-- {-# INLINE foldr' #-}
-- foldr' = G.foldr'

-- -- | /O(n)/ Pure right fold using a function applied to each element and its index.
-- --
-- -- @since 0.12.3.0
-- ifoldr :: (Int -> a -> b -> b) -> b -> STVector s a -> m b
-- {-# INLINE ifoldr #-}
-- ifoldr = G.ifoldr

-- -- | /O(n)/ Pure right fold with strict accumulator using a function applied
-- -- to each element and its index.
-- --
-- -- @since 0.12.3.0
-- ifoldr' :: (Int -> a -> b -> b) -> b -> STVector s a -> m b
-- {-# INLINE ifoldr' #-}
-- ifoldr' = G.ifoldr'

-- -- | /O(n)/ Monadic fold.
-- --
-- -- @since 0.12.3.0
-- foldM :: (b -> a -> m b) -> b -> STVector s a -> m b
-- {-# INLINE foldM #-}
-- foldM = G.foldM

-- -- | /O(n)/ Monadic fold with strict accumulator.
-- --
-- -- @since 0.12.3.0
-- foldM' :: (b -> a -> m b) -> b -> STVector s a -> m b
-- {-# INLINE foldM' #-}
-- foldM' = G.foldM'

-- -- | /O(n)/ Monadic fold using a function applied to each element and its index.
-- --
-- -- @since 0.12.3.0
-- ifoldM :: (b -> Int -> a -> m b) -> b -> STVector s a -> m b
-- {-# INLINE ifoldM #-}
-- ifoldM = G.ifoldM

-- -- | /O(n)/ Monadic fold with strict accumulator using a function applied to each element and its index.
-- --
-- -- @since 0.12.3.0
-- ifoldM' :: (b -> Int -> a -> m b) -> b -> STVector s a -> m b
-- {-# INLINE ifoldM' #-}
-- ifoldM' = G.ifoldM'

-- -- | /O(n)/ Monadic right fold.
-- --
-- -- @since 0.12.3.0
-- foldrM :: (a -> b -> m b) -> b -> STVector s a -> m b
-- {-# INLINE foldrM #-}
-- foldrM = G.foldrM

-- -- | /O(n)/ Monadic right fold with strict accumulator.
-- --
-- -- @since 0.12.3.0
-- foldrM' :: (a -> b -> m b) -> b -> STVector s a -> m b
-- {-# INLINE foldrM' #-}
-- foldrM' = G.foldrM'

-- -- | /O(n)/ Monadic right fold using a function applied to each element and its index.
-- --
-- -- @since 0.12.3.0
-- ifoldrM :: (Int -> a -> b -> m b) -> b -> STVector s a -> m b
-- {-# INLINE ifoldrM #-}
-- ifoldrM = G.ifoldrM

-- -- | /O(n)/ Monadic right fold with strict accumulator using a function applied
-- -- to each element and its index.
-- --
-- -- @since 0.12.3.0
-- ifoldrM' :: (Int -> a -> b -> m b) -> b -> STVector s a -> m b
-- {-# INLINE ifoldrM' #-}
-- ifoldrM' = G.ifoldrM'

data STVectorSlice s a = UnsafeSTVectorSlice {-# UNPACK #-} !(STVector s a) !Int !Int

whole :: STVector s a -> STVectorSlice s a
whole m = UnsafeSTVectorSlice m 0 (length m)

unsafeTakeL :: Int -> STVectorSlice s a -> STVectorSlice s a
unsafeTakeL n (UnsafeSTVectorSlice m off _) = UnsafeSTVectorSlice m off n

unsafeTakeR :: Int -> STVectorSlice s a -> STVectorSlice s a
unsafeTakeR n (UnsafeSTVectorSlice m off len) = UnsafeSTVectorSlice m (off + len - n) n

unsafeDropL :: Int -> STVectorSlice s a -> STVectorSlice s a
unsafeDropL n (UnsafeSTVectorSlice m off len) = UnsafeSTVectorSlice m (off + n) (len - n)

unsafeDropR :: Int -> STVectorSlice s a -> STVectorSlice s a
unsafeDropR n (UnsafeSTVectorSlice m off len) = UnsafeSTVectorSlice m off (len - n)

-- -- $setup
-- -- >>> import Prelude (Integer,Num(..),($))
