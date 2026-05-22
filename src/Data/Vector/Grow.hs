{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE TypeApplications #-}
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
-- Mutable boxed vectors.

module Data.Vector.Grow (
  -- * Mutable boxed vectors
  GrowVector (..),
  GrowVector_ (..),

  -- ** Length information
  length, capacity,

  -- ** Initialisation
  new, 
  -- unsafeNew,
  -- clone,

  -- * Accessing individual elements
  read, readMaybe, write, 
  unsafeRead, unsafeWrite, 

  -- * Growing
  pushBack,

  -- -- ** Filling and copying
  -- set, copy, move, unsafeCopy, unsafeMove,

  -- -- ** Slice
  -- GrowVectorSlice (..), whole, unsafeTakeL, unsafeTakeR, unsafeDropL, unsafeDropR,
) where

import Control.Monad.ST

import qualified Data.Vector.Mutable as M

import Prelude( Eq (..), Ord (..), Bool, Ordering(..), Int, Maybe, (<$>), error, otherwise, (&&), (||), pure, Maybe (..), Num (..), Monad (..))
import qualified GHC.Exts as GHC
import qualified GHC.ST as GHC
import Data.STRef
import Unsafe.Coerce (unsafeCoerceUnlifted)
import Data.Elevator

data GrowVector s a = UnsafeGrowVector {-# UNPACK #-}
  !(STRef s (GrowVector_ s a))

data GrowVector_ s a = UnsafeGrowVector_ 
  !Int {-# UNPACK #-} !(M.STVector s a)

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: GrowVector s a -> ST s Int
{-# INLINE length #-}
length (UnsafeGrowVector ref) = do
  UnsafeGrowVector_ n _ <- readSTRef ref
  return n

capacity :: GrowVector s a -> ST s Int
capacity (UnsafeGrowVector ref) = do
  UnsafeGrowVector_ _ (M.UnsafeSTVector m) <- readSTRef ref
  GHC.ST (\s ->
    case GHC.getSizeofSmallMutableArray# m s of
      (# s', n #) -> (# s', GHC.I# n #))

-- Initialisation
-- --------------

-- | Create a grow vector of the given length.
--
-- @since 0.5
new :: ST s (GrowVector s a)
{-# INLINE new #-}
new = do
  m <- M.unsafeNew 3
  ref <- newSTRef (UnsafeGrowVector_ 0 m)
  return (UnsafeGrowVector ref)

-- -- | Create a copy of a mutable vector.
-- clone :: GrowVector s a -> ST s (GrowVector s a)
-- {-# INLINE clone #-}
-- clone (UnsafeGrowVector marr) = GHC.ST (\s -> 
--   case GHC.cloneSmallMutableArray# marr 0# (GHC.sizeofSmallMutableArray# marr) s of
--     (# s', marr' #) -> (# s', UnsafeGrowVector marr' #))

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
read :: GrowVector s a -> Int -> ST s a
{-# INLINE read #-}
read (UnsafeGrowVector ref) i = do
  UnsafeGrowVector_ n m <- readSTRef ref
  if 0 <= i && i < n
    then M.unsafeRead m i
    else error "read: index out of bounds"

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
readMaybe :: GrowVector s a -> Int -> ST s (Maybe a)
{-# INLINE readMaybe #-}
readMaybe (UnsafeGrowVector ref) i = do
  UnsafeGrowVector_ n m <- readSTRef ref
  if 0 <= i && i < n
    then Just <$> M.unsafeRead m i
    else return Nothing

-- | Replace the element at the given position.
write :: GrowVector s a -> Int -> a -> ST s ()
{-# INLINE write #-}
write (UnsafeGrowVector ref) i x = do
  UnsafeGrowVector_ n m <- readSTRef ref
  if 0 <= i && i < n
    then M.unsafeWrite m i x
    else error "write: index out of bounds"

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: GrowVector s a -> Int -> ST s a
{-# INLINE unsafeRead #-}
unsafeRead (UnsafeGrowVector ref) i = do
  UnsafeGrowVector_ _ m <- readSTRef ref
  M.unsafeRead m i

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: GrowVector s a -> Int -> a -> ST s ()
{-# INLINE unsafeWrite #-}
unsafeWrite (UnsafeGrowVector ref) i x = do
  UnsafeGrowVector_ _ m <- readSTRef ref
  M.unsafeWrite m i x

-- Growing
-- -------

pushBack :: GrowVector s a -> a -> ST s ()
pushBack (UnsafeGrowVector ref) x = do
  UnsafeGrowVector_ i m@(M.UnsafeSTVector marr) <- readSTRef ref
  GHC.I# n <- GHC.ST (\s -> 
    case GHC.getSizeofSmallMutableArray# marr s of
      (# s', n #) -> (# s', GHC.I# n #))
  m' <- if i < GHC.I# n then return m else do
    GHC.ST (\s ->
      case GHC.newSmallArray# (2# GHC.*# n) (Strict x) s of { (# s', marr' #) ->
      case GHC.copySmallMutableArray# marr 0# marr' 0# n s' of { s'' ->
      (# s'', M.UnsafeSTVector marr' #)}})
  M.unsafeWrite m' i x
  writeSTRef ref (UnsafeGrowVector_ (i + 1) m')

-- -- Filling and copying
-- -- -------------------

-- -- | Set all elements of the vector to the given value.
-- set :: GrowVector s a -> a -> ST s ()
-- {-# INLINE set #-}
-- set = G.set

-- -- | Copy a vector. The two vectors must have the same length and may not
-- -- overlap.
-- copy :: GrowVector s a   -- ^ target
--      -> GrowVector s a   -- ^ source
--      -> ST s ()
-- {-# INLINE copy #-}
-- copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap, but this is not checked.
-- unsafeCopy :: GrowVector s a   -- ^ target
--            -> GrowVector s a   -- ^ source
--            -> ST s ()
-- {-# INLINE unsafeCopy #-}
-- unsafeCopy (UnsafeGrowVector marr') (UnsafeGrowVector marr) = GHC.ST (\s ->
--   case 
--   _)

-- -- | Move the contents of a vector. The two vectors must have the same
-- -- length.
-- --
-- -- If the vectors do not overlap, then this is equivalent to 'copy'.
-- -- Otherwise, the copying is performed as if the source vector were
-- -- copied to a temporary vector and then the temporary vector was copied
-- -- to the target vector.
-- move :: GrowVector s a   -- ^ target
--                     -> GrowVector s a   -- ^ source
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
-- unsafeMove :: GrowVector s a   -- ^ target
--                           -> GrowVector s a   -- ^ source
--                           -> ST s ()
-- {-# INLINE unsafeMove #-}
-- unsafeMove = G.unsafeMove

-- $setup
-- >>> import Prelude (Integer,Num(..),($))
