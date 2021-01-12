{-# LANGUAGE NamedFieldPuns #-}
module Unix.IOVec
    ( CIOVec(..)
    , unsafeFromByteString
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import Zhp
import qualified Data.ByteString.Internal as BS

#include <sys/uio.h>

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
