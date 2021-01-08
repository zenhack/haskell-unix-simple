module Unix
    ( preadBuf, preadBufExn
    , pread
    , pwriteBuf, pwriteBufExn
    , readBuf, readBufExn
    , writeBuf, writeBufExn
    -- , read, readExn
    , write, writeExn
    ) where

import Foreign.C.Error
import Foreign.ForeignPtr
import Foreign.Ptr        (plusPtr)
import Unix.C
import Unix.C.Errors
import Unix.Errors
import Zhp

import qualified Data.ByteString.Internal as BS

type EIO a = IO (Either Errno a)

preadBuf :: Fd -> Ptr Word8 -> CSize -> COff -> EIO CSsize
preadBuf fd ptr sz off =
    orErrno $ c_pread fd ptr sz off

pread :: Fd -> CSize -> COff -> EIO BS.ByteString
pread fd sz off = do
    fptr <- mallocForeignPtrBytes (fromIntegral sz)
    r <- withForeignPtr fptr $ \ptr -> preadBuf fd ptr sz off
    pure $! case r of
        Left e  -> Left e
        Right v -> Right (BS.fromForeignPtr fptr 0 (fromIntegral v))

preadBufExn :: Fd -> Ptr Word8 -> CSize -> COff -> IO CSsize
preadBufExn fd ptr sz off =
    throwIfErrno $ preadBuf fd ptr sz off

pwriteBuf :: Fd -> Ptr Word8 -> CSize -> COff -> EIO CSsize
pwriteBuf fd ptr sz off =
    orErrno $ c_pwrite fd ptr sz off

pwriteBufExn :: Fd -> Ptr Word8 -> CSize -> COff -> IO CSsize
pwriteBufExn fd ptr sz off =
    throwIfErrno $ pwriteBuf fd ptr sz off

readBuf :: Fd -> Ptr Word8 -> CSize -> EIO CSsize
readBuf fd ptr sz =
    orErrno $ c_read fd ptr sz

readBufExn :: Fd -> Ptr Word8 -> CSize -> IO CSsize
readBufExn fd ptr sz =
    throwIfErrno $ readBuf fd ptr sz

read :: Fd -> Int -> EIO BS.ByteString
read fd sz = do
    fptr <- mallocForeignPtrBytes sz
    r <- withForeignPtr fptr $ \ptr -> readBuf fd ptr (fromIntegral sz)
    -- TODO(perf): should we trim the buffer to the size actually
    -- read? Maybe if it's below a certain size,
    -- possibly relative to the size of the buffer?
    pure $! case r of
        Left e  -> Left e
        Right v -> Right (BS.fromForeignPtr fptr 0 (fromIntegral v))

readExn :: Fd -> Int -> IO BS.ByteString
readExn fd sz =
    throwIfErrno $ read fd sz

writeBuf :: Fd -> Ptr Word8 -> CSize -> EIO CSsize
writeBuf fd ptr sz =
    orErrno $ c_write fd ptr sz

writeBufExn :: Fd -> Ptr Word8 -> CSize -> IO CSsize
writeBufExn fd ptr sz =
    throwIfErrno $ writeBuf fd ptr sz

write :: Fd -> BS.ByteString -> EIO CSsize
write fd bs =
    let (fptr, off, len) = BS.toForeignPtr bs in
    withForeignPtr fptr $ \ptr ->
        writeBuf fd (plusPtr ptr off) (fromIntegral len)

writeExn :: Fd -> BS.ByteString -> IO CSsize
writeExn fd bs =
    throwIfErrno $ write fd bs
