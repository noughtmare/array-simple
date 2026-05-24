{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
-- |
-- Module      : Data.Array.Simple.Mutable
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
-- Strict mutable arrays.

module Data.Array.Simple.Mutable (
  -- * Mutable boxed arrays
  STArray (..),

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
  STArraySlice (..), whole, unsafeTakeL, unsafeTakeR, unsafeDropL, unsafeDropR,
) where

import qualified GHC.Exts as GHC
import qualified GHC.ST as GHC
import Control.Monad.ST ( ST )
import qualified Unsafe.Coerce

import Prelude( Eq (..), Ord (..), Bool, Int, Maybe, error, otherwise, (&&), pure, Maybe (..), Num (..), Monad (..))

-- | A mutable array that is strict in its elements.
-- This type takes up /2 + n/ words of memory, where /n/ is the number of elements.
data STArray s a = UnsafeSTArray {-# UNPACK #-} !(GHC.SmallMutableArray# s a)

-- Length information
-- ------------------

-- | Length of the mutable array.
length :: STArray s a -> Int
{-# INLINE length #-}
length (UnsafeSTArray m) = GHC.I# (GHC.sizeofSmallMutableArray# m)

-- | Check whether the array is empty.
null :: STArray s a -> Bool
{-# INLINE null #-}
null m = length m == 0

-- Initialisation
-- --------------

-- | Create a mutable array of the given length.
new :: Int -> a -> ST s (STArray s a)
{-# INLINE new #-}
new (GHC.I# n) !x = GHC.ST (\s -> 
  case GHC.newSmallArray# n x s of { (# s', marr #) ->
    (# s', UnsafeSTArray marr #)
  })

-- | Create a mutable array of the given length. The array elements
-- are set to an undefined value, so accessing them will cause a segfault at best.
unsafeNew :: Int -> ST s (STArray s a)
{-# INLINE unsafeNew #-}
unsafeNew (GHC.I# n) = GHC.ST (\s ->
  let !x = Unsafe.Coerce.unsafeCoerce () in
  case GHC.newSmallArray# n x s of { (# s', marr #) ->
    (# s', UnsafeSTArray marr #)
  })

-- | Create a copy of a mutable array.
clone :: STArray s a -> ST s (STArray s a)
{-# INLINE clone #-}
clone (UnsafeSTArray marr) = GHC.ST (\s -> 
  case GHC.cloneSmallMutableArray# marr 0# (GHC.sizeofSmallMutableArray# marr) s of
    (# s', marr' #) -> (# s', UnsafeSTArray marr' #))

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position. Will throw an exception if
-- the index is out of range.
read :: STArray s a -> Int -> ST s a
{-# INLINE read #-}
read m i | 0 <= i && i < length m = unsafeRead m i
         | otherwise = error "read: index out of bounds"

-- | Yield the element at the given position. Returns 'Nothing' if
-- the index is out of range.
readMaybe :: STArray s a -> Int -> ST s (Maybe a)
{-# INLINE readMaybe #-}
readMaybe m i 
  | 0 <= i && i < length m = do
    !x <- unsafeRead m i
    return (Just x)
  | otherwise = pure Nothing

-- | Replace the element at the given position.
write :: STArray s a -> Int -> a -> ST s ()
{-# INLINE write #-}
write m i x
  | 0 <= i && i < length m = unsafeWrite m i x
  | otherwise = error "write: index out of bounds"

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: STArray s a -> Int -> ST s a
{-# INLINE unsafeRead #-}
unsafeRead (UnsafeSTArray m) (GHC.I# i) = GHC.ST (\s -> 
  case GHC.readSmallArray# m i s of
    (# s', !x #) -> (# s', x #))

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: STArray s a -> Int -> a -> ST s ()
{-# INLINE unsafeWrite #-}
unsafeWrite (UnsafeSTArray m) (GHC.I# i) !x = GHC.ST (\s ->
  (# GHC.writeSmallArray# m i x s , () #))

-- Shrinking
-- ---------

-- | Shrink the array. Can throw an exception if the new length is out of 
-- bounds.
shrink :: STArray s a -> Int -> ST s ()
shrink m n
  | 0 <= n && n < length m = unsafeShrink m n
  | otherwise = error "shrink: new length out of bounds"

-- | Shrink the array without checking if the new size is in bounds. 
unsafeShrink :: STArray s a -> Int -> ST s ()
unsafeShrink (UnsafeSTArray m) (GHC.I# n) = GHC.ST (\s ->
  case GHC.shrinkSmallMutableArray# m n s of
    s' -> (# s', () #))

-- -- Filling and copying
-- -- -------------------

-- -- | Set all elements of the array to the given value.
-- set :: STArray s a -> a -> ST s ()
-- {-# INLINE set #-}
-- set = G.set

-- -- | Copy a array. The two arrays must have the same length and may not
-- -- overlap.
-- copy :: STArray s a   -- ^ target
--      -> STArray s a   -- ^ source
--      -> ST s ()
-- {-# INLINE copy #-}
-- copy = G.copy

-- | Copy a array. The two arrays must have the same length and may not
-- overlap, but this is not checked.
-- unsafeCopy :: STArray s a   -- ^ target
--            -> STArray s a   -- ^ source
--            -> ST s ()
-- {-# INLINE unsafeCopy #-}
-- unsafeCopy (UnsafeSTArray marr') (UnsafeSTArray marr) = GHC.ST (\s ->
--   case 
--   _)

-- -- | Move the contents of a array. The two arrays must have the same
-- -- length.
-- --
-- -- If the arrays do not overlap, then this is equivalent to 'copy'.
-- -- Otherwise, the copying is performed as if the source array were
-- -- copied to a temporary array and then the temporary array was copied
-- -- to the target array.
-- move :: STArray s a   -- ^ target
--                     -> STArray s a   -- ^ source
--                     -> ST s ()
-- {-# INLINE move #-}
-- move = G.move

-- -- | Move the contents of a array. The two arrays must have the same
-- -- length, but this is not checked.
-- --
-- -- If the arrays do not overlap, then this is equivalent to 'unsafeCopy'.
-- -- Otherwise, the copying is performed as if the source array were
-- -- copied to a temporary array and then the temporary array was copied
-- -- to the target array.
-- unsafeMove :: STArray s a   -- ^ target
--                           -> STArray s a   -- ^ source
--                           -> ST s ()
-- {-# INLINE unsafeMove #-}
-- unsafeMove = G.unsafeMove

-- Slicing
-- -------

-- | A slice (subarray) of a mutable array. This takes up 2 extra words, so /4 + n/ words total.
data STArraySlice s a = UnsafeSTArraySlice {-# UNPACK #-} !(STArray s a) !Int !Int

-- | Convert a array to a slice which covers the whole array.
whole :: STArray s a -> STArraySlice s a
whole m = UnsafeSTArraySlice m 0 (length m)

-- | Take a prefix of a slice
unsafeTakeL :: Int -> STArraySlice s a -> STArraySlice s a
unsafeTakeL n (UnsafeSTArraySlice m off _) = UnsafeSTArraySlice m off n

-- | Take a suffix of a slice
unsafeTakeR :: Int -> STArraySlice s a -> STArraySlice s a
unsafeTakeR n (UnsafeSTArraySlice m off len) = UnsafeSTArraySlice m (off + len - n) n

-- | Remove a prefix of a slice
unsafeDropL :: Int -> STArraySlice s a -> STArraySlice s a
unsafeDropL n (UnsafeSTArraySlice m off len) = UnsafeSTArraySlice m (off + n) (len - n)

-- | Remove a suffix of a slice
unsafeDropR :: Int -> STArraySlice s a -> STArraySlice s a
unsafeDropR n (UnsafeSTArraySlice m off len) = UnsafeSTArraySlice m off (len - n)

-- $setup
-- >>> import Prelude (Integer,Num(..),($))
