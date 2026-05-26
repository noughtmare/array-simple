{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
-- |
-- Module      : Data.Array.Simple.Unboxed.Grow
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
-- Growable unboxed arrays. In particular the 'pushBack' operation allows you
-- to create a array one element at a time without knowing the final array 
-- length. It automatically doubles the capacity of the grow array when 
-- needed.
--
-- This is optimized for constructing an immutable 'Data.Array.Simple.Array', not for prolongued
-- use as a mutable array.
module Data.Array.Simple.Unboxed.Grow (
  -- * Grow array type
  GrowUArray (..),
  GrowUArray_ (..),

  -- * Length information
  length, capacity,

  -- * Initialisation
  new, 
  -- unsafeNew,
  -- clone,

  -- * Accessing individual elements
  read, readMaybe, write, 
  unsafeRead, unsafeWrite, 

  -- * Growing
  pushBack,

  -- -- * Filling and copying
  -- set, copy, move, unsafeCopy, unsafeMove,
) where

import Control.Monad.ST ( ST )

import qualified Data.Array.Simple.Unboxed.Mutable as M

import Data.Array.Byte
import Data.Array.Simple.Unboxed.Class (Unbox)

import Prelude( Ord (..), Int, Maybe, error, (&&), Maybe (..), Num (..), Monad (..))
import qualified GHC.Exts as GHC
import qualified GHC.ST as GHC
import Data.STRef ( newSTRef, readSTRef, writeSTRef, STRef )

-- | The main grow array type.
data GrowUArray s a = UnsafeGrowUArray {-# UNPACK #-} !(STRef s (GrowUArray_ s a))

-- | Internal grow array record. Not intended for general use.
-- Invariant: the Int must be non-negative and smaller than the size of the 
-- 'Data.Array.Mutable.STArray' (the latter can change over time).
data GrowUArray_ s a = UnsafeGrowUArray_ !Int {-# UNPACK #-} !(M.STUArray s a)

-- Length information
-- ------------------

-- | Length (number of elements) in the grow array.
length :: GrowUArray s a -> ST s Int
{-# INLINE length #-}
length (UnsafeGrowUArray ref) = do
  UnsafeGrowUArray_ n _ <- readSTRef ref
  return n

-- | Total number of slots in the grow array.
capacity :: Unbox a => GrowUArray s a -> ST s Int
capacity (UnsafeGrowUArray ref) = do
  UnsafeGrowUArray_ _ m <- readSTRef ref
  M.length m

-- Initialisation
-- --------------

-- | Create a grow array of the given length.
new :: Unbox a => ST s (GrowUArray s a)
{-# INLINE new #-}
new = do
  m <- M.new 3
  ref <- newSTRef (UnsafeGrowUArray_ 0 m)
  return (UnsafeGrowUArray ref)

-- -- | Create a copy of a mutable array.
-- clone :: GrowUArray s a -> ST s (GrowUArray s a)
-- {-# INLINE clone #-}
-- clone (UnsafeGrowUArray marr) = GHC.ST (\s -> 
--   case GHC.cloneSmallMutableArray# marr 0# (GHC.sizeofSmallMutableArray# marr) s of
--     (# s', marr' #) -> (# s', UnsafeGrowUArray marr' #))

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position. Will throw an exception if
-- the index is out of range.
read :: Unbox a => GrowUArray s a -> Int -> ST s a
{-# INLINE read #-}
read (UnsafeGrowUArray ref) i = do
  UnsafeGrowUArray_ n m <- readSTRef ref
  if 0 <= i && i < n
    then M.unsafeRead m i
    else error "read: index out of bounds"

-- | Yield the element at the given position. Returns 'Nothing' if
-- the index is out of range.
readMaybe :: Unbox a => GrowUArray s a -> Int -> ST s (Maybe a)
{-# INLINE readMaybe #-}
readMaybe (UnsafeGrowUArray ref) i = do
  UnsafeGrowUArray_ n m <- readSTRef ref
  if 0 <= i && i < n
    then do
      !x <- M.unsafeRead m i
      return (Just x)
    else return Nothing

-- | Replace the element at the given position.
write :: Unbox a => GrowUArray s a -> Int -> a -> ST s ()
{-# INLINE write #-}
write (UnsafeGrowUArray ref) i x = do
  UnsafeGrowUArray_ n m <- readSTRef ref
  if 0 <= i && i < n
    then M.unsafeWrite m i x
    else error "write: index out of bounds"

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: Unbox a => GrowUArray s a -> Int -> ST s a
{-# INLINE unsafeRead #-}
unsafeRead (UnsafeGrowUArray ref) i = do
  UnsafeGrowUArray_ _ m <- readSTRef ref
  M.unsafeRead m i

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: Unbox a => GrowUArray s a -> Int -> a -> ST s ()
{-# INLINE unsafeWrite #-}
unsafeWrite (UnsafeGrowUArray ref) i x = do
  UnsafeGrowUArray_ _ m <- readSTRef ref
  M.unsafeWrite m i x

-- Growing
-- -------

unI# :: Int -> GHC.Int#
unI# (GHC.I# x) = x

-- | Write an element to the end (right) of the grow array. Doubles the 
-- capacity of the array if necessary.
pushBack :: Unbox a => GrowUArray s a -> a -> ST s ()
{-# INLINE pushBack #-}
pushBack (UnsafeGrowUArray ref) !x = do
  UnsafeGrowUArray_ i m@(M.UnsafeSTUArray (MutableByteArray marr)) <- readSTRef ref
  n <- M.length m
  m' <- if i < n then return m else do
    m'@(M.UnsafeSTUArray (MutableByteArray marr')) <- M.new (2 * n)
    GHC.ST (\s ->
      case GHC.copyMutableByteArray# marr 0# marr' 0# (unI# n) s of { s' ->
      (# s', m' #)})
  M.unsafeWrite m' i x
  writeSTRef ref (UnsafeGrowUArray_ (i + 1) m')

-- -- Filling and copying
-- -- -------------------

-- -- | Set all elements of the array to the given value.
-- set :: GrowUArray s a -> a -> ST s ()
-- {-# INLINE set #-}
-- set = G.set

-- -- | Copy a array. The two arrays must have the same length and may not
-- -- overlap.
-- copy :: GrowUArray s a   -- ^ target
--      -> GrowUArray s a   -- ^ source
--      -> ST s ()
-- {-# INLINE copy #-}
-- copy = G.copy

-- | Copy a array. The two arrays must have the same length and may not
-- overlap, but this is not checked.
-- unsafeCopy :: GrowUArray s a   -- ^ target
--            -> GrowUArray s a   -- ^ source
--            -> ST s ()
-- {-# INLINE unsafeCopy #-}
-- unsafeCopy (UnsafeGrowUArray marr') (UnsafeGrowUArray marr) = GHC.ST (\s ->
--   case 
--   _)

-- -- | Move the contents of a array. The two arrays must have the same
-- -- length.
-- --
-- -- If the arrays do not overlap, then this is equivalent to 'copy'.
-- -- Otherwise, the copying is performed as if the source array were
-- -- copied to a temporary array and then the temporary array was copied
-- -- to the target array.
-- move :: GrowUArray s a   -- ^ target
--                     -> GrowUArray s a   -- ^ source
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
-- unsafeMove :: GrowUArray s a   -- ^ target
--                           -> GrowUArray s a   -- ^ source
--                           -> ST s ()
-- {-# INLINE unsafeMove #-}
-- unsafeMove = G.unsafeMove

-- $setup
-- >>> import Prelude (Integer,Num(..),($))
