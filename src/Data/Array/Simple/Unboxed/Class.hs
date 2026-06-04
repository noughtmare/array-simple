{-# LANGUAGE MagicHash, UnboxedTuples, ExtendedLiterals #-}
{-# LANGUAGE FunctionalDependencies, TypeFamilies, RequiredTypeArguments, DataKinds, TypeAbstractions, UndecidableInstances #-}
module Data.Array.Simple.Unboxed.Class where

import qualified GHC.Exts as GHC
import GHC.Exts (Int#, Double(D#), Float (F#))
import Data.Bits
import Data.Array.Byte ( ByteArray(..), MutableByteArray (MutableByteArray) )
import Control.Monad.ST ( ST )
import qualified GHC.ST as GHC
import GHC.Word
import GHC.Int
import Data.Kind
import qualified Unsafe.Coerce

unI# :: Int -> Int#
unI# (I# x) = x

class Unbox a where
    sizeOf :: forall b -> (a ~ b) => Int
    -- | In logBase 2, so alignment a = 3 means a can only be indexed at multiples of 8 bytes.
    alignment :: forall b -> (a ~ b) => Int
    -- | Indexed by bytes, must satisfy alignment
    unsafeIndex :: ByteArray -> Int -> a
    -- | Indexed by bytes, must satisfy alignment
    unsafeRead :: MutableByteArray s -> Int -> ST s a
    -- | Indexed by bytes, must satisfy alignment
    unsafeWrite :: MutableByteArray s -> Int -> a -> ST s ()

instance Unbox Int8 where
    sizeOf Int8 = 1
    {-# INLINE sizeOf #-}
    alignment Int8 = 0
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> I8# (GHC.indexInt8Array# arr (unI# (i `unsafeShiftR` alignment Int8)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readInt8Array# marr (unI# (i `unsafeShiftR` alignment Int8)) s of (# s', x #) -> (# s', I8# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (I8# x) -> GHC.ST (\s -> (# GHC.writeInt8Array# marr (unI# (i `unsafeShiftR` alignment Int8)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance Unbox Int16 where
    sizeOf Int16 = 2
    {-# INLINE sizeOf #-}
    alignment Int16 = 1
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> I16# (GHC.indexInt16Array# arr (unI# (i `unsafeShiftR` alignment Int16)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readInt16Array# marr (unI# (i `unsafeShiftR` alignment Int16)) s of (# s', x #) -> (# s', I16# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (I16# x) -> GHC.ST (\s -> (# GHC.writeInt16Array# marr (unI# (i `unsafeShiftR` alignment Int16)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance Unbox Int32 where
    sizeOf Int32 = 4
    {-# INLINE sizeOf #-}
    alignment Int32 = 2
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> I32# (GHC.indexInt32Array# arr (unI# (i `unsafeShiftR` alignment Int32)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readInt32Array# marr (unI# (i `unsafeShiftR` alignment Int32)) s of (# s', x #) -> (# s', I32# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (I32# x) -> GHC.ST (\s -> (# GHC.writeInt32Array# marr (unI# (i `unsafeShiftR` alignment Int32)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance Unbox Int64 where
    sizeOf Int64 = 8
    {-# INLINE sizeOf #-}
    alignment Int64 = 3
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> I64# (GHC.indexInt64Array# arr (unI# (i `unsafeShiftR` alignment Int64)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readInt64Array# marr (unI# (i `unsafeShiftR` alignment Int64)) s of (# s', x #) -> (# s', I64# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (I64# x) -> GHC.ST (\s -> (# GHC.writeInt64Array# marr (unI# (i `unsafeShiftR` alignment Int64)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance Unbox Int where
    sizeOf Int = 8
    {-# INLINE sizeOf #-}
    alignment Int = 3
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> I# (GHC.indexIntArray# arr (unI# (i `unsafeShiftR` alignment Int)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readIntArray# marr (unI# (i `unsafeShiftR` alignment Int)) s of (# s', x #) -> (# s', I# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (I# x) -> GHC.ST (\s -> (# GHC.writeIntArray# marr (unI# (i `unsafeShiftR` alignment Int)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance Unbox Double where
    sizeOf Double = 8
    {-# INLINE sizeOf #-}
    alignment Double = 3
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> D# (GHC.indexDoubleArray# arr (unI# (i `unsafeShiftR` alignment Double)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readDoubleArray# marr (unI# (i `unsafeShiftR` alignment Double)) s of (# s', x #) -> (# s', D# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (D# x) -> GHC.ST (\s -> (# GHC.writeDoubleArray# marr (unI# (i `unsafeShiftR` alignment Double)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance Unbox Float where
    sizeOf Float = 4
    {-# INLINE sizeOf #-}
    alignment Float = 2
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> F# (GHC.indexFloatArray# arr (unI# (i `unsafeShiftR` alignment Float)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readFloatArray# marr (unI# (i `unsafeShiftR` alignment Float)) s of (# s', x #) -> (# s', F# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (F# x) -> GHC.ST (\s -> (# GHC.writeFloatArray# marr (unI# (i `unsafeShiftR` alignment Float)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance Unbox Word8 where
    sizeOf Word8 = 1
    {-# INLINE sizeOf #-}
    alignment Word8 = 0
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> W8# (GHC.indexWord8Array# arr (unI# (i `unsafeShiftR` alignment Word8)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readWord8Array# marr (unI# (i `unsafeShiftR` alignment Word8)) s of (# s', x #) -> (# s', W8# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (W8# x) -> GHC.ST (\s -> (# GHC.writeWord8Array# marr (unI# (i `unsafeShiftR` alignment Word8)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance Unbox Word16 where
    sizeOf Word16 = 2
    {-# INLINE sizeOf #-}
    alignment Word16 = 1
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> W16# (GHC.indexWord16Array# arr (unI# (i `unsafeShiftR` alignment Word16)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readWord16Array# marr (unI# (i `unsafeShiftR` alignment Word16)) s of (# s', x #) -> (# s', W16# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (W16# x) -> GHC.ST (\s -> (# GHC.writeWord16Array# marr (unI# (i `unsafeShiftR` alignment Word16)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance Unbox Word32 where
    sizeOf Word32 = 4
    {-# INLINE sizeOf #-}
    alignment Word32 = 2
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> W32# (GHC.indexWord32Array# arr (unI# (i `unsafeShiftR` alignment Word32)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readWord32Array# marr (unI# (i `unsafeShiftR` alignment Word32)) s of (# s', x #) -> (# s', W32# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (W32# x) -> GHC.ST (\s -> (# GHC.writeWord32Array# marr (unI# (i `unsafeShiftR` alignment Word32)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance Unbox Word64 where
    sizeOf Word64 = 8
    {-# INLINE sizeOf #-}
    alignment Word64 = 3
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> W64# (GHC.indexWord64Array# arr (unI# (i `unsafeShiftR` alignment Word64)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readWord64Array# marr (unI# (i `unsafeShiftR` alignment Word64)) s of (# s', x #) -> (# s', W64# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (W64# x) -> GHC.ST (\s -> (# GHC.writeWord64Array# marr (unI# (i `unsafeShiftR` alignment Word64)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance Unbox Word where
    sizeOf Word = 8
    {-# INLINE sizeOf #-}
    alignment Word = 3
    {-# INLINE alignment #-}
    unsafeIndex (ByteArray arr) = \i -> W# (GHC.indexWordArray# arr (unI# (i `unsafeShiftR` alignment Word64)))
    {-# INLINE unsafeIndex #-}
    unsafeRead (MutableByteArray marr) = \i -> GHC.ST (\s -> case GHC.readWordArray# marr (unI# (i `unsafeShiftR` alignment Word64)) s of (# s', x #) -> (# s', W# x #))
    {-# INLINE unsafeRead #-}
    unsafeWrite (MutableByteArray marr) = \i (W# x) -> GHC.ST (\s -> (# GHC.writeWordArray# marr (unI# (i `unsafeShiftR` alignment Word64)) x s, () #))
    {-# INLINE unsafeWrite #-}

instance (Unbox a, Unbox b) => Unbox (a, b) where
    sizeOf _ = sizeOf (type (Tuple [a, b])) 
    {-# INLINE sizeOf #-}
    alignment _ = alignment (type (Tuple [a, b]))
    {-# INLINE alignment #-}
    unsafeIndex ba = \i -> case unsafeIndex @(Tuple [a, b]) ba i of
        TCons x (TCons y TNil) -> (x, y)
    {-# INLINE unsafeIndex #-}
    unsafeRead mba = \i -> do
        x <- unsafeRead @(Tuple [a, b]) mba i
        case x of
            TCons x1 (TCons x2 TNil) -> return (x1, x2)
    {-# INLINE unsafeRead #-}
    unsafeWrite mba = \i (x, y) -> unsafeWrite mba i (TCons x (TCons y TNil))
    {-# INLINE unsafeWrite #-}

instance (Unbox a, Unbox b, Unbox c) => Unbox (a, b, c) where
    sizeOf _ = sizeOf (type (Tuple [a, b, c])) 
    {-# INLINE sizeOf #-}
    alignment _ = alignment (type (Tuple [a, b, c]))
    {-# INLINE alignment #-}
    unsafeIndex ba = \i -> case unsafeIndex @(Tuple [a, b, c]) ba i of
        TCons x (TCons y (TCons z TNil)) -> (x, y, z)
    {-# INLINE unsafeIndex #-}
    unsafeRead mba = \i -> do
        tup <- unsafeRead @(Tuple [a, b, c]) mba i
        case tup of
            TCons x (TCons y (TCons z TNil)) -> return (x, y, z)
    {-# INLINE unsafeRead #-}
    unsafeWrite mba = \i (x, y, z) -> unsafeWrite mba i (TCons x (TCons y (TCons z TNil)))
    {-# INLINE unsafeWrite #-}

instance (Unbox a, Unbox b) => Unbox (Either a b) where
    sizeOf _ = sizeOf (type (Sum [a, b])) 
    {-# INLINE sizeOf #-}
    alignment _ = alignment (type (Sum [a, b]))
    {-# INLINE alignment #-}
    unsafeIndex ba = \i -> case unsafeIndex @(Sum [a, b]) ba i of
        UnsafeSum 0 x -> Left (Unsafe.Coerce.unsafeCoerce @GHC.Any @a x)
        UnsafeSum _ x -> Right (Unsafe.Coerce.unsafeCoerce @GHC.Any @b x)
    {-# INLINE unsafeIndex #-}
    unsafeRead mba = \i -> do
        s <- unsafeRead @(Sum [a, b]) mba i
        case s of
            UnsafeSum 0 x -> return (Left (Unsafe.Coerce.unsafeCoerce @GHC.Any @a x))
            UnsafeSum _ x -> return (Right (Unsafe.Coerce.unsafeCoerce @GHC.Any @b x))
    {-# INLINE unsafeRead #-}
    unsafeWrite mba i (Left x) = unsafeWrite @(Sum [a, b]) mba i (UnsafeSum 0 (Unsafe.Coerce.unsafeCoerce @a @GHC.Any x))
    unsafeWrite mba i (Right x) = unsafeWrite @(Sum [a, b]) mba i (UnsafeSum 1 (Unsafe.Coerce.unsafeCoerce @b @GHC.Any x))
    {-# INLINE unsafeWrite #-}

instance Unbox () where
    sizeOf _ = 0
    {-# INLINE sizeOf #-}
    alignment _ = 0
    {-# INLINE alignment #-}
    unsafeIndex _ _ = ()
    {-# INLINE unsafeIndex #-}
    unsafeRead _ _ = return ()
    {-# INLINE unsafeRead #-}
    unsafeWrite _ _ _ = return ()
    {-# INLINE unsafeWrite #-}

instance Unbox Bool where
    sizeOf _ = sizeOf Word8
    {-# INLINE sizeOf #-}
    alignment _ = alignment Word8
    {-# INLINE alignment #-}
    unsafeIndex ba = \i -> case unsafeIndex @Word8 ba i of
        0 -> False
        _ -> True
    {-# INLINE unsafeIndex #-}
    unsafeRead mba = \i -> do
        x <- unsafeRead @Word8 mba i
        case x of
            0 -> return False
            _ -> return True
    {-# INLINE unsafeRead #-}
    unsafeWrite mba i False = unsafeWrite @Word8 mba i 0
    unsafeWrite mba i True = unsafeWrite @Word8 mba i 1
    {-# INLINE unsafeWrite #-}

instance Unbox a => Unbox (Maybe a) where
    sizeOf _ = sizeOf (type (Sum [(), a])) 
    {-# INLINE sizeOf #-}
    alignment _ = alignment (type (Sum [(), a]))
    {-# INLINE alignment #-}
    unsafeIndex ba = \i -> case unsafeIndex @(Sum [(), a]) ba i of
        UnsafeSum 0 _ -> Nothing
        UnsafeSum _ x -> Just (Unsafe.Coerce.unsafeCoerce @GHC.Any @a x)
    {-# INLINE unsafeIndex #-}
    unsafeRead mba = \i -> do
        s <- unsafeRead @(Sum [(), a]) mba i
        case s of
            UnsafeSum 0 _ -> return Nothing
            UnsafeSum _ x -> return (Just (Unsafe.Coerce.unsafeCoerce @GHC.Any @a x))
    {-# INLINE unsafeRead #-}
    unsafeWrite mba i Nothing = unsafeWrite @(Sum [(), a]) mba i (UnsafeSum 0 (Unsafe.Coerce.unsafeCoerce @() @GHC.Any ()))
    unsafeWrite mba i (Just x) = unsafeWrite @(Sum [(), a]) mba i (UnsafeSum 1 (Unsafe.Coerce.unsafeCoerce @a @GHC.Any x))
    {-# INLINE unsafeWrite #-}

-- FIXME: sizeOf (type (Word8, (Word8, Word32))) = 12 where it could be just 8
-- I now think C also does this if you nest two structs

type Tuple :: [Type] -> Type
data Tuple xs where
    TNil :: Tuple '[]
    TCons :: !a -> !(Tuple as) -> Tuple (a : as)

instance Unbox (Tuple '[]) where
    sizeOf (Tuple []) = 0
    {-# INLINE sizeOf #-}
    alignment (Tuple []) = 0
    {-# INLINE alignment #-}
    unsafeIndex _ _ = TNil
    {-# INLINE unsafeIndex #-}
    unsafeRead _ _ = return TNil
    {-# INLINE unsafeRead #-}
    unsafeWrite _ _ _ = return ()
    {-# INLINE unsafeWrite #-}

instance Unbox a => Unbox (Tuple '[a]) where
    sizeOf _ = sizeOf a
    {-# INLINE sizeOf #-}
    alignment _ = alignment a
    {-# INLINE alignment #-}
    unsafeIndex ba = \i -> TCons (unsafeIndex ba i) TNil
    {-# INLINE unsafeIndex #-}
    unsafeRead mba = \i -> do
        x <- unsafeRead mba i
        return (TCons x TNil)
    {-# INLINE unsafeRead #-}
    unsafeWrite mba = \i (TCons x TNil) -> unsafeWrite mba i x
    {-# INLINE unsafeWrite #-}

instance(Unbox a, Unbox b, Unbox (Tuple (b:cs))) => Unbox (Tuple (a : b : cs)) where
    sizeOf _ = (sizeOf a + (l - 1) .&. (- l)) + sizeOf (Tuple (b:cs)) where
        l = 1 `unsafeShiftL` alignment (type b)
    {-# INLINE sizeOf #-}
    alignment _ = max (alignment a) (alignment (Tuple (b:cs)))
    {-# INLINE alignment #-}
    unsafeIndex ba = \i -> TCons (unsafeIndex ba i) (unsafeIndex ba (i + (sizeOf a + (l - 1) .&. (- l)))) where
        l = 1 `unsafeShiftL` alignment (type b)
    {-# INLINE unsafeIndex #-}
    unsafeRead mba = \i -> do
        x <- unsafeRead mba i
        xs <- unsafeRead mba (i + (sizeOf a + (l - 1) .&. (- l)))
        return (TCons x xs)
        where
        l = 1 `unsafeShiftL` alignment (type b)
    {-# INLINE unsafeRead #-}
    unsafeWrite mba = \i (TCons x xs) -> do
        unsafeWrite mba i x
        unsafeWrite mba (i + (sizeOf a + (l - 1) .&. (- l))) xs
        where
        l = 1 `unsafeShiftL` alignment (type b)
    {-# INLINE unsafeWrite #-}

type Sum :: [Type] -> Type
data Sum as where
    UnsafeSum :: !Int32 -> !(GHC.Any @Type) -> Sum as

class UnboxUnion as where
    sizeOfUnion :: forall bs -> (bs ~ as) => Int
    alignmentUnion :: forall bs -> (bs ~ as) => Int
    unsafeIndexUnion :: ByteArray -> Int -> Int32 -> Sum as
    unsafeReadUnion :: MutableByteArray s -> Int -> Int32 -> ST s (Sum as)
    unsafeWriteUnion :: MutableByteArray s -> Int -> Sum as -> ST s ()

weaken :: Sum as -> Sum (a : as)
weaken (UnsafeSum tag x) = UnsafeSum (tag + 1) x

instance Unbox a => UnboxUnion '[a] where
    sizeOfUnion _ = sizeOf a
    {-# INLINE sizeOfUnion #-}
    alignmentUnion _ = alignment a
    {-# INLINE alignmentUnion #-}
    unsafeIndexUnion ba i 0 = UnsafeSum 0 (Unsafe.Coerce.unsafeCoerce @a @GHC.Any (unsafeIndex ba i))
    unsafeIndexUnion _ _ _ = error "unsafeIndexUnion: tag out of bounds" 
    {-# INLINE unsafeIndexUnion #-}
    unsafeReadUnion mba i 0  = do
        x <- unsafeRead mba i
        return (UnsafeSum 0 (Unsafe.Coerce.unsafeCoerce @a @GHC.Any x))
    unsafeReadUnion _ _ _ = error "unsafeReadUnion: tag out of bounds"
    {-# INLINE unsafeReadUnion #-}
    unsafeWriteUnion mba = \i (UnsafeSum _ x) -> do
        unsafeWrite mba i (Unsafe.Coerce.unsafeCoerce @GHC.Any @a x)
    {-# INLINE unsafeWriteUnion #-}

instance (Unbox a, UnboxUnion (b : as)) => UnboxUnion (a : b : as) where
    sizeOfUnion _ = max (sizeOf a) (sizeOfUnion (b : as))
    {-# INLINE sizeOfUnion #-}
    alignmentUnion _ = max (alignment a) (alignmentUnion (b : as))
    {-# INLINE alignmentUnion #-}
    unsafeIndexUnion ba i 0 = UnsafeSum 0 (Unsafe.Coerce.unsafeCoerce @a @GHC.Any (unsafeIndex ba i))
    unsafeIndexUnion ba i tag = weaken (unsafeIndexUnion ba i (tag - 1))
    {-# INLINE unsafeIndexUnion #-}
    unsafeReadUnion mba i 0  = do
        x <- unsafeRead mba i
        return (UnsafeSum 0 (Unsafe.Coerce.unsafeCoerce @a @GHC.Any x))
    unsafeReadUnion mba i tag = do
        x <- unsafeReadUnion mba i (tag - 1)
        return (weaken x)
    {-# INLINE unsafeReadUnion #-}
    unsafeWriteUnion mba i (UnsafeSum 0 x) = do
        unsafeWrite mba i (Unsafe.Coerce.unsafeCoerce @GHC.Any @a x)
    unsafeWriteUnion mba i (UnsafeSum tag x) = do
        unsafeWriteUnion @(b : as) mba i (UnsafeSum (tag - 1) x)
    {-# INLINE unsafeWriteUnion #-}

instance (UnboxUnion as) => Unbox (Sum as) where
    sizeOf _ = (sizeOf Int32 + (l - 1) .&. (- l)) + sizeOfUnion as where
        l = 1 `unsafeShiftL` alignmentUnion as
    {-# INLINE sizeOf #-}
    alignment _ = max (alignment Int32) (alignmentUnion as)
    {-# INLINE alignment #-}
    unsafeIndex ba i =
        let tag = unsafeIndex ba i in
        unsafeIndexUnion ba (i + (sizeOf Int32 + (l - 1) .&. (- l))) tag
        where
        l = 1 `unsafeShiftL` alignmentUnion as
    {-# INLINE unsafeIndex #-}
    unsafeRead mba = \i -> do
        tag <- unsafeRead mba i
        unsafeReadUnion mba (i + (sizeOf Int32 + (l - 1) .&. (- l))) tag
        where
        l = 1 `unsafeShiftL` alignmentUnion as
    {-# INLINE unsafeRead #-}
    unsafeWrite mba = \i s@(UnsafeSum tag _) -> do
        unsafeWrite mba i tag
        unsafeWriteUnion mba (i + (sizeOf Int32 + (l - 1) .&. (- l))) s
        where
        l = 1 `unsafeShiftL` alignmentUnion as
    {-# INLINE unsafeWrite #-}
