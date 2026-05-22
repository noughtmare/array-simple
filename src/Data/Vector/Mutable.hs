{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
-- |
-- Module      : Data.Vector.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
--                   Alexey Kuleshevich 2020-2022
--                   Aleksey Khudyakov 2020-2022
--                   Andrew Lelechenko 2020-2022
--                   Jaro Reinders 2026
-- License     : BSD-style
--
-- Maintainer  : Jaro Reinders <code@jaro.addy.io>
-- Stability   : experimental
-- Portability : non-portable
--
-- Strict mutable vectors.

module Data.Vector.Mutable (
  -- * Mutable boxed vectors
  STVector (..),

  -- ** Length information
  length, null,

  -- ** Initialisation
  new, unsafeNew,
  clone,

  -- * Accessing individual elements
  read, readMaybe, write, 
  unsafeRead, unsafeWrite,

  -- * Shrinking
  shrink, unsafeShrink, 

  -- -- ** Filling and copying
  -- set, copy, move, unsafeCopy, unsafeMove,

  -- ** Slice (unstable)
  STVectorSlice (..), whole, unsafeTakeL, unsafeTakeR, unsafeDropL, unsafeDropR,
) where

import Data.Elevator ( UnliftedType, Strict(..) )
import qualified GHC.Exts as GHC
import qualified GHC.ST as GHC
import Control.Monad.ST ( ST )
import qualified Unsafe.Coerce

import Prelude( Eq (..), Ord (..), Bool, Int, Maybe, (<$>), error, otherwise, (&&), pure, Maybe (..), Num (..))

-- | A mutable vector that is strict in its elements.
-- This type takes up /2 + n/ words of memory, where /n/ is the number of elements.
data STVector s a = UnsafeSTVector {-# UNPACK #-} !(GHC.SmallMutableArray# s (Strict a))

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: STVector s a -> Int
{-# INLINE length #-}
length (UnsafeSTVector m) = GHC.I# (GHC.sizeofSmallMutableArray# m)

-- | Check whether the vector is empty.
null :: STVector s a -> Bool
{-# INLINE null #-}
null m = length m == 0

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: Int -> a -> ST s (STVector s a)
{-# INLINE new #-}
new (GHC.I# n) x = GHC.ST (\s -> 
  case GHC.newSmallArray# n (Strict x) s of { (# s', marr #) ->
    (# s', UnsafeSTVector marr #)
  })

type UnliftedUnit :: UnliftedType
data UnliftedUnit = U

-- | Create a mutable vector of the given length. The vector elements
-- are set to an undefined value, so accessing them will cause a segfault at best.
unsafeNew :: Int -> ST s (STVector s a)
{-# INLINE unsafeNew #-}
unsafeNew (GHC.I# n) = GHC.ST (\s ->
  case GHC.newSmallArray# n (Unsafe.Coerce.unsafeCoerceUnlifted U) s of { (# s', marr #) ->
    (# s', UnsafeSTVector marr #)
  })

-- | Create a copy of a mutable vector.
clone :: STVector s a -> ST s (STVector s a)
{-# INLINE clone #-}
clone (UnsafeSTVector marr) = GHC.ST (\s -> 
  case GHC.cloneSmallMutableArray# marr 0# (GHC.sizeofSmallMutableArray# marr) s of
    (# s', marr' #) -> (# s', UnsafeSTVector marr' #))

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position. Will throw an exception if
-- the index is out of range.
read :: STVector s a -> Int -> ST s a
{-# INLINE read #-}
read m i | 0 <= i && i < length m = unsafeRead m i
         | otherwise = error "read: index out of bounds"

-- | Yield the element at the given position. Returns 'Nothing' if
-- the index is out of range.
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

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: STVector s a -> Int -> ST s a
{-# INLINE unsafeRead #-}
unsafeRead (UnsafeSTVector m) (GHC.I# i) = GHC.ST (\s -> 
  case GHC.readSmallArray# m i s of
    (# s', Strict x #) -> (# s', x #))

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: STVector s a -> Int -> a -> ST s ()
{-# INLINE unsafeWrite #-}
unsafeWrite (UnsafeSTVector m) (GHC.I# i) x = GHC.ST (\s ->
  (# GHC.writeSmallArray# m i (Strict x) s , () #))

-- Shrinking
-- ---------

-- | Shrink the vector. Can throw an exception if the new length is out of 
-- bounds.
shrink :: STVector s a -> Int -> ST s ()
shrink m n
  | 0 <= n && n < length m = unsafeShrink m n
  | otherwise = error "shrink: new length out of bounds"

-- | Shrink the vector without checking if the new size is in bounds. 
unsafeShrink :: STVector s a -> Int -> ST s ()
unsafeShrink (UnsafeSTVector m) (GHC.I# n) = GHC.ST (\s ->
  case GHC.shrinkSmallMutableArray# m n s of
    s' -> (# s', () #))

-- -- Filling and copying
-- -- -------------------

-- -- | Set all elements of the vector to the given value.
-- set :: STVector s a -> a -> ST s ()
-- {-# INLINE set #-}
-- set = G.set

-- -- | Copy a vector. The two vectors must have the same length and may not
-- -- overlap.
-- copy :: STVector s a   -- ^ target
--      -> STVector s a   -- ^ source
--      -> ST s ()
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

-- Slicing
-- -------

-- | A slice (subvector) of a mutable vector. This takes up 2 extra words, so /4 + n/ words total.
data STVectorSlice s a = UnsafeSTVectorSlice {-# UNPACK #-} !(STVector s a) !Int !Int

-- | Convert a vector to a slice which covers the whole vector.
whole :: STVector s a -> STVectorSlice s a
whole m = UnsafeSTVectorSlice m 0 (length m)

-- | Take a prefix of a slice
unsafeTakeL :: Int -> STVectorSlice s a -> STVectorSlice s a
unsafeTakeL n (UnsafeSTVectorSlice m off _) = UnsafeSTVectorSlice m off n

-- | Take a suffix of a slice
unsafeTakeR :: Int -> STVectorSlice s a -> STVectorSlice s a
unsafeTakeR n (UnsafeSTVectorSlice m off len) = UnsafeSTVectorSlice m (off + len - n) n

-- | Remove a prefix of a slice
unsafeDropL :: Int -> STVectorSlice s a -> STVectorSlice s a
unsafeDropL n (UnsafeSTVectorSlice m off len) = UnsafeSTVectorSlice m (off + n) (len - n)

-- | Remove a suffix of a slice
unsafeDropR :: Int -> STVectorSlice s a -> STVectorSlice s a
unsafeDropR n (UnsafeSTVectorSlice m off len) = UnsafeSTVectorSlice m off (len - n)

-- $setup
-- >>> import Prelude (Integer,Num(..),($))
