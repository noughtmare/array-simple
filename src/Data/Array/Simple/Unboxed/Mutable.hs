{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes, RequiredTypeArguments, TypeAbstractions #-}
{-# LANGUAGE RoleAnnotations #-}
-- |
-- Module      : Data.Array.Simple.Unboxed.Mutable
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
-- Unboxed mutable arrays.

module Data.Array.Simple.Unboxed.Mutable (
  -- * Mutable boxed arrays
  STUArray (..),

  -- ** Length information
  length,

  -- ** Initialisation
  new,

  -- * Accessing individual elements
  read, readMaybe, write, 
  unsafeRead, unsafeWrite,

  -- * Shrinking
  shrink, unsafeShrink, 

  -- -- ** Filling and copying
  -- set, copy, move, unsafeCopy, unsafeMove,

  -- ** Slice (unstable)
  -- STUArraySlice (..), whole, unsafeTakeL, unsafeTakeR, unsafeDropL, unsafeDropR,
) where

import qualified Data.Array.Simple.Unboxed.Class as Class
import Data.Array.Simple.Unboxed.Class (Unbox)
import qualified GHC.Exts as GHC
import qualified GHC.ST as GHC
import Control.Monad.ST ( ST )
import Data.Array.Byte
import Data.Kind

import Prelude( Eq (..), Ord (..), Bool, Int, Maybe, error, otherwise, (&&), pure, Maybe (..), Num (..), Monad (..), quot)

-- | A mutable array that is strict in its elements.
-- This type takes up /2 + n/ words of memory, where /n/ is the number of elements.
type STUArray :: Type -> Type -> Type
type role STUArray nominal nominal
newtype STUArray s a = UnsafeSTUArray (MutableByteArray s)

-- Length information
-- ------------------

-- | Length of the mutable array.
length :: Unbox a => STUArray s a -> ST s Int
{-# INLINE length #-}
length @a (UnsafeSTUArray (MutableByteArray m)) = GHC.ST (\s -> 
  case GHC.getSizeofMutableByteArray# m s of
    (# s', x #) -> (# s', GHC.I# x `quot` Class.sizeOf a #))

-- Initialisation
-- --------------

unI# :: Int -> GHC.Int#
unI# (GHC.I# x) = x

-- | Create a mutable array of the given length.
new :: Unbox a => Int -> ST s (STUArray s a)
{-# INLINE new #-}
new @a n = GHC.ST (\s -> 
  case GHC.newByteArray# (unI# (n * Class.sizeOf a)) s of { (# s', marr #) ->
    (# s', UnsafeSTUArray (MutableByteArray marr) #)
  })

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position. Will throw an exception if
-- the index is out of range.
read :: Unbox a => STUArray s a -> Int -> ST s a
{-# INLINE read #-}
read m i = do
  n <- length m
  if 0 <= i && i < n
    then unsafeRead m i
    else error "read: index out of bounds"

-- | Yield the element at the given position. Returns 'Nothing' if
-- the index is out of range.
readMaybe :: Unbox a => STUArray s a -> Int -> ST s (Maybe a)
{-# INLINE readMaybe #-}
readMaybe m i = do
  n <- length m
  if 0 <= i && i < n then do
    !x <- unsafeRead m i
    return (Just x)
  else pure Nothing

-- | Replace the element at the given position.
write :: Unbox a => STUArray s a -> Int -> a -> ST s ()
{-# INLINE write #-}
write m i x = do
  n <- length m
  if 0 <= i && i < n
    then unsafeWrite m i x
    else error "write: index out of bounds"

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: Unbox a => STUArray s a -> Int -> ST s a
{-# INLINE unsafeRead #-}
unsafeRead (UnsafeSTUArray m) i = Class.unsafeRead m i

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: Unbox a => STUArray s a -> Int -> a -> ST s ()
{-# INLINE unsafeWrite #-}
unsafeWrite (UnsafeSTUArray m) i !x = Class.unsafeWrite m i x

-- Shrinking
-- ---------

-- | Shrink the array. Can throw an exception if the new length is out of
-- bounds.
shrink :: Unbox a => STUArray s a -> Int -> ST s ()
shrink m n = do
  k <- length m
  if 0 <= n && n < k
    then unsafeShrink m n
    else error "shrink: new length out of bounds"

-- | Shrink the array without checking if the new size is in bounds. 
unsafeShrink :: Unbox a => STUArray s a -> Int -> ST s ()
unsafeShrink @a (UnsafeSTUArray (MutableByteArray m)) n = GHC.ST (\s ->
  case GHC.shrinkMutableByteArray# m (unI# (n * Class.sizeOf a))  s of
    s' -> (# s', () #))

-- -- Filling and copying
-- -- -------------------

-- -- | Set all elements of the array to the given value.
-- set :: STUArray s a -> a -> ST s ()
-- {-# INLINE set #-}
-- set = G.set

-- -- | Copy a array. The two arrays must have the same length and may not
-- -- overlap.
-- copy :: STUArray s a   -- ^ target
--      -> STUArray s a   -- ^ source
--      -> ST s ()
-- {-# INLINE copy #-}
-- copy = G.copy

-- | Copy a array. The two arrays must have the same length and may not
-- overlap, but this is not checked.
-- unsafeCopy :: STUArray s a   -- ^ target
--            -> STUArray s a   -- ^ source
--            -> ST s ()
-- {-# INLINE unsafeCopy #-}
-- unsafeCopy (UnsafeSTUArray marr') (UnsafeSTUArray marr) = GHC.ST (\s ->
--   case 
--   _)

-- -- | Move the contents of a array. The two arrays must have the same
-- -- length.
-- --
-- -- If the arrays do not overlap, then this is equivalent to 'copy'.
-- -- Otherwise, the copying is performed as if the source array were
-- -- copied to a temporary array and then the temporary array was copied
-- -- to the target array.
-- move :: STUArray s a   -- ^ target
--                     -> STUArray s a   -- ^ source
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
-- unsafeMove :: STUArray s a   -- ^ target
--                           -> STUArray s a   -- ^ source
--                           -> ST s ()
-- {-# INLINE unsafeMove #-}
-- unsafeMove = G.unsafeMove

-- -- Slicing
-- -- -------

-- -- | A slice (subarray) of a mutable array. This takes up 2 extra words, so /4 + n/ words total.
-- data STUArraySlice s a = UnsafeSTUArraySlice {-# UNPACK #-} !(STUArray s a) !Int !Int

-- -- | Convert a array to a slice which covers the whole array.
-- whole :: STUArray s a -> STUArraySlice s a
-- whole m = UnsafeSTUArraySlice m 0 (length m)

-- -- | Take a prefix of a slice
-- unsafeTakeL :: Int -> STUArraySlice s a -> STUArraySlice s a
-- unsafeTakeL n (UnsafeSTUArraySlice m off _) = UnsafeSTUArraySlice m off n

-- -- | Take a suffix of a slice
-- unsafeTakeR :: Int -> STUArraySlice s a -> STUArraySlice s a
-- unsafeTakeR n (UnsafeSTUArraySlice m off len) = UnsafeSTUArraySlice m (off + len - n) n

-- -- | Remove a prefix of a slice
-- unsafeDropL :: Int -> STUArraySlice s a -> STUArraySlice s a
-- unsafeDropL n (UnsafeSTUArraySlice m off len) = UnsafeSTUArraySlice m (off + n) (len - n)

-- -- | Remove a suffix of a slice
-- unsafeDropR :: Int -> STUArraySlice s a -> STUArraySlice s a
-- unsafeDropR n (UnsafeSTUArraySlice m off len) = UnsafeSTUArraySlice m off (len - n)

-- $setup
-- >>> import Prelude (Integer,Num(..),($))
