{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
-- |
-- Module      : Data.Array.Simple.Grow
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
-- Growable arrays. In particular the 'pushBack' operation allows you
-- to create a array one element at a time without knowing the final array 
-- length. It automatically doubles the capacity of the grow array when 
-- needed.
--
-- This is optimized for constructing an immutable 'Data.Array.Simple.Array', not for prolongued
-- use as a mutable array.
module Data.Array.Simple.Grow (
  -- * Grow array type
  GrowArray (..),
  GrowArray_ (..),

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

import qualified Data.Array.Simple.Mutable as M

import Prelude( Ord (..), Int, Maybe, (<$>), error, (&&), Maybe (..), Num (..), Monad (..))
import qualified GHC.Exts as GHC
import qualified GHC.ST as GHC
import Data.STRef ( newSTRef, readSTRef, writeSTRef, STRef )
import Data.Elevator ( Strict(Strict) )

-- | The main grow array type.
data GrowArray s a = UnsafeGrowArray {-# UNPACK #-}
  !(STRef s (GrowArray_ s a))

-- | Internal grow array record. Not intended for general use.
-- Invariant: the Int must be non-negative and smaller than the size of the 
-- 'Data.Array.Mutable.STArray' (the latter can change over time).
data GrowArray_ s a = UnsafeGrowArray_ 
  !Int {-# UNPACK #-} !(M.STArray s a)

-- Length information
-- ------------------

-- | Length (number of elements) in the grow array.
length :: GrowArray s a -> ST s Int
{-# INLINE length #-}
length (UnsafeGrowArray ref) = do
  UnsafeGrowArray_ n _ <- readSTRef ref
  return n

-- | Total number of slots in the grow array.
capacity :: GrowArray s a -> ST s Int
capacity (UnsafeGrowArray ref) = do
  UnsafeGrowArray_ _ (M.UnsafeSTArray m) <- readSTRef ref
  GHC.ST (\s ->
    case GHC.getSizeofSmallMutableArray# m s of
      (# s', n #) -> (# s', GHC.I# n #))

-- Initialisation
-- --------------

-- | Create a grow array of the given length.
new :: ST s (GrowArray s a)
{-# INLINE new #-}
new = do
  m <- M.unsafeNew 3
  ref <- newSTRef (UnsafeGrowArray_ 0 m)
  return (UnsafeGrowArray ref)

-- -- | Create a copy of a mutable array.
-- clone :: GrowArray s a -> ST s (GrowArray s a)
-- {-# INLINE clone #-}
-- clone (UnsafeGrowArray marr) = GHC.ST (\s -> 
--   case GHC.cloneSmallMutableArray# marr 0# (GHC.sizeofSmallMutableArray# marr) s of
--     (# s', marr' #) -> (# s', UnsafeGrowArray marr' #))

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position. Will throw an exception if
-- the index is out of range.
read :: GrowArray s a -> Int -> ST s a
{-# INLINE read #-}
read (UnsafeGrowArray ref) i = do
  UnsafeGrowArray_ n m <- readSTRef ref
  if 0 <= i && i < n
    then M.unsafeRead m i
    else error "read: index out of bounds"

-- | Yield the element at the given position. Returns 'Nothing' if
-- the index is out of range.
readMaybe :: GrowArray s a -> Int -> ST s (Maybe a)
{-# INLINE readMaybe #-}
readMaybe (UnsafeGrowArray ref) i = do
  UnsafeGrowArray_ n m <- readSTRef ref
  if 0 <= i && i < n
    then Just <$> M.unsafeRead m i
    else return Nothing

-- | Replace the element at the given position.
write :: GrowArray s a -> Int -> a -> ST s ()
{-# INLINE write #-}
write (UnsafeGrowArray ref) i x = do
  UnsafeGrowArray_ n m <- readSTRef ref
  if 0 <= i && i < n
    then M.unsafeWrite m i x
    else error "write: index out of bounds"

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: GrowArray s a -> Int -> ST s a
{-# INLINE unsafeRead #-}
unsafeRead (UnsafeGrowArray ref) i = do
  UnsafeGrowArray_ _ m <- readSTRef ref
  M.unsafeRead m i

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: GrowArray s a -> Int -> a -> ST s ()
{-# INLINE unsafeWrite #-}
unsafeWrite (UnsafeGrowArray ref) i x = do
  UnsafeGrowArray_ _ m <- readSTRef ref
  M.unsafeWrite m i x

-- Growing
-- -------

-- | Write an element to the end (right) of the grow array. Doubles the 
-- capacity of the array if necessary.
pushBack :: GrowArray s a -> a -> ST s ()
{-# INLINE pushBack #-}
pushBack (UnsafeGrowArray ref) x = do
  UnsafeGrowArray_ i m@(M.UnsafeSTArray marr) <- readSTRef ref
  GHC.I# n <- GHC.ST (\s -> 
    case GHC.getSizeofSmallMutableArray# marr s of
      (# s', n #) -> (# s', GHC.I# n #))
  m' <- if i < GHC.I# n then return m else do
    GHC.ST (\s ->
      case GHC.newSmallArray# (2# GHC.*# n) (Strict x) s of { (# s', marr' #) ->
      case GHC.copySmallMutableArray# marr 0# marr' 0# n s' of { s'' ->
      (# s'', M.UnsafeSTArray marr' #)}})
  M.unsafeWrite m' i x
  writeSTRef ref (UnsafeGrowArray_ (i + 1) m')

-- -- Filling and copying
-- -- -------------------

-- -- | Set all elements of the array to the given value.
-- set :: GrowArray s a -> a -> ST s ()
-- {-# INLINE set #-}
-- set = G.set

-- -- | Copy a array. The two arrays must have the same length and may not
-- -- overlap.
-- copy :: GrowArray s a   -- ^ target
--      -> GrowArray s a   -- ^ source
--      -> ST s ()
-- {-# INLINE copy #-}
-- copy = G.copy

-- | Copy a array. The two arrays must have the same length and may not
-- overlap, but this is not checked.
-- unsafeCopy :: GrowArray s a   -- ^ target
--            -> GrowArray s a   -- ^ source
--            -> ST s ()
-- {-# INLINE unsafeCopy #-}
-- unsafeCopy (UnsafeGrowArray marr') (UnsafeGrowArray marr) = GHC.ST (\s ->
--   case 
--   _)

-- -- | Move the contents of a array. The two arrays must have the same
-- -- length.
-- --
-- -- If the arrays do not overlap, then this is equivalent to 'copy'.
-- -- Otherwise, the copying is performed as if the source array were
-- -- copied to a temporary array and then the temporary array was copied
-- -- to the target array.
-- move :: GrowArray s a   -- ^ target
--                     -> GrowArray s a   -- ^ source
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
-- unsafeMove :: GrowArray s a   -- ^ target
--                           -> GrowArray s a   -- ^ source
--                           -> ST s ()
-- {-# INLINE unsafeMove #-}
-- unsafeMove = G.unsafeMove

-- $setup
-- >>> import Prelude (Integer,Num(..),($))
