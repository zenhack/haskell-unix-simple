{-# LANGUAGE NamedFieldPuns #-}
-- | Utilities for working with @struct iovec@.
module Unix.IOVec
    ( CIOVec(..)
    , dropBytes
    , useIOVecs
    , unsafeFromByteString
    , unsafeWithByteStrings
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable.Mutable as SMV
import Zhp
import qualified Data.ByteString.Internal as BS

#include <sys/uio.h>

-- | A @struct iovec@
data CIOVec = CIOVec
    { iovBase :: !(Ptr Word8)
    , iovLen :: !CSize
    }

-- | Unsafe O(1) conversion from a bytestring. This is unsafe in two
-- respects:
--
-- 1. Since the underlying storage is shared with the original bytestring,
--    if the CIOVec is mutated, then the bytestring will be as well, violating
--    referential transparency.
-- 2. Holding a reference to the CIOVec does not keep the underlying storage
--    from being garbage collected, so you must use touchForeignPtr on the
--    underlying foreign pointer after the last use of the CIOVec.
unsafeFromByteString :: BS.ByteString -> CIOVec
unsafeFromByteString bs =
    let (fptr, off, len) = BS.toForeignPtr bs in
    unsafePerformIO $ withForeignPtr fptr $ \ptr ->
        pure $ CIOVec { iovBase = plusPtr ptr off, iovLen = fromIntegral len }

useIOVecs :: SMV.IOVector CIOVec -> (Ptr CIOVec -> IO a) -> IO a
useIOVecs vecs f =
    let (fptr, _, _) = SMV.unsafeToForeignPtr vecs in
    withForeignPtr fptr f

-- | Borrow a list of bytestrings as an @'SMV.IOVector' 'CIOVec'@, which can
-- be passed somewhat directly to c functions that expect a @struct iovec *@
-- and a length.
--
-- This is unsafe, in that:
--
-- 1. Since the underlying storage is shared with the original bytestring,
--    if a CIOVec is mutated, then the bytestring will be as well, violating
--    referential transparency.
-- 2. If the values are retained after the function returns, this might result
--    in use-after-free errors.
unsafeWithByteStrings :: [BS.ByteString] -> (SMV.IOVector CIOVec -> IO a) -> IO a
unsafeWithByteStrings bss f = do
    vec <- SMV.new (length bss)
    for_ (zip [0..] bss) $ \(i, bs) ->
        SMV.write vec i (unsafeFromByteString bs)
    result <- f vec
    for_ bss $ \bs ->
        let (fptr, _, _) = BS.toForeignPtr bs in
        touchForeignPtr fptr
    pure result

-- | Drop the specified number of bytes from the front of the vector, returning
-- the result. Note that this may also modify the vector in-place.
dropBytes :: CSize -> SMV.IOVector CIOVec -> IO (SMV.IOVector CIOVec)
dropBytes n vec
    | SMV.length vec == 0 = pure vec
    | otherwise = do
        hd@CIOVec{iovLen, iovBase} <- SMV.read vec 0
        if iovLen <= n
            then dropBytes (n - iovLen) (SMV.tail vec)
            else (do
                SMV.write vec 0 CIOVec
                    { iovLen = iovLen - n
                    , iovBase = plusPtr iovBase (fromIntegral n)
                    }
                pure vec)


instance Storable CIOVec where
    alignment _ = #{alignment struct iovec}
    sizeOf _ = #{size struct iovec}
    peek ptr = do
        iovBase <- #{peek struct iovec, iov_base} ptr
        iovLen <- #{peek struct iovec, iov_len} ptr
        pure CIOVec {iovBase, iovLen}
    poke ptr v = do
        #{poke struct iovec, iov_base} ptr (iovBase v)
        #{poke struct iovec, iov_len} ptr (iovLen v)
