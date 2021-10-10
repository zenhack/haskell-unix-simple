{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE BangPatterns #-}
module Unix
    ( fsync, fsyncExn
    , fdatasync, fdatasyncExn
    , ftruncate, ftruncateExn
    , mkdir, mkdirExn
    , preadBuf, preadBufExn
    , pread, preadExn
    , pwriteBuf, pwriteBufExn
    , pwrite, pwriteExn
    , pwriteFull, pwriteFullExn
    , readBuf, readBufExn
    , writeBuf, writeBufExn
    , read, readExn
    , remove, removeExn
    , rmdir, rmdirExn
    , write, writeExn
    , writeFull, writeFullExn

    , OpenFlag(..)
    , open, openExn
    , openat, openatExn
    , o_APPEND
    , o_CLOEXEC
    , o_CREAT
    , o_DIRECTORY
    , o_EXCL
    , o_NOFOLLOW
    , o_NONBLOCK
    , o_NDELAY
    , o_TRUNC
    , o_RDONLY
    , o_WRONLY
    , o_RDWR

    , close, closeExn

    -- * Re-exported for convenience
    , CString
    ) where

import CString
import Foreign.C.Error
import Foreign.ForeignPtr
import Unix.C
import Unix.C.Errors
import Unix.Errors
import Unix.IOVec
import Zhp

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Internal     as BS
import qualified Data.Vector.Storable.Mutable as SMV

type EIO a = IO (Either Errno a)

fsync :: Fd -> EIO ()
fsync fd = retryEINTR $ orErrno $ void $ c_fsync fd

fsyncExn :: Fd -> IO ()
fsyncExn fd = throwIfErrno $ fsync fd

fdatasync :: Fd -> EIO ()
fdatasync fd = retryEINTR $ orErrno $ void $ c_fdatasync fd

fdatasyncExn :: Fd -> IO ()
fdatasyncExn fd = throwIfErrno $ fdatasync fd

ftruncate :: Fd -> COff -> EIO ()
ftruncate fd off = retryEINTR $ orErrno $ void $ c_ftruncate fd off

ftruncateExn :: Fd -> COff -> IO ()
ftruncateExn fd off =
    throwIfErrno $ ftruncate fd off

preadBuf :: Fd -> Ptr Word8 -> CSize -> COff -> EIO CSsize
preadBuf fd ptr sz off =
    retryEINTR $ orErrno $ c_pread fd ptr sz off

preadBufExn :: Fd -> Ptr Word8 -> CSize -> COff -> IO CSsize
preadBufExn fd ptr sz off =
    throwIfErrno $ preadBuf fd ptr sz off

pread :: Fd -> CSize -> COff -> EIO BS.ByteString
pread fd sz off = do
    fptr <- mallocForeignPtrBytes (fromIntegral sz)
    r <- withForeignPtr fptr $ \ptr -> preadBuf fd ptr sz off
    pure $! case r of
        Left e  -> Left e
        Right v -> Right (BS.fromForeignPtr fptr 0 (fromIntegral v))

preadExn :: Fd -> CSize -> COff -> IO BS.ByteString
preadExn fd sz off = throwIfErrno $ pread fd sz off

pwriteBuf :: Fd -> Ptr Word8 -> CSize -> COff -> EIO CSsize
pwriteBuf fd ptr sz off =
    retryEINTR $ orErrno $ c_pwrite fd ptr sz off

pwriteBufExn :: Fd -> Ptr Word8 -> CSize -> COff -> IO CSsize
pwriteBufExn fd ptr sz off =
    throwIfErrno $ pwriteBuf fd ptr sz off

pwrite :: Fd -> BS.ByteString -> COff -> EIO CSsize
pwrite fd bs off =
    let (fptr, foff, len) = BS.toForeignPtr bs in
    withForeignPtr fptr $ \ptr ->
        pwriteBuf fd (plusPtr ptr foff) (fromIntegral len) off

pwriteExn :: Fd -> BS.ByteString -> COff -> IO CSsize
pwriteExn fd bs off = throwIfErrno $ pwrite fd bs off

pwriteFull :: Fd -> BS.ByteString -> COff -> EIO ()
pwriteFull fd bs off = do
    ret <- pwrite fd bs off
    case ret of
        Left e -> pure $ Left e
        Right v
            | fromIntegral v == BS.length bs ->
                pure $ Right ()
            | otherwise ->
                pwriteFull fd
                    (BS.drop (fromIntegral v) bs)
                    (off + fromIntegral v)

pwriteFullExn :: Fd -> BS.ByteString -> COff -> IO ()
pwriteFullExn fd bs off = throwIfErrno $ pwriteFull fd bs off

readBuf :: Fd -> Ptr Word8 -> CSize -> EIO CSsize
readBuf fd ptr sz =
    retryEINTR $ orErrno $ c_read fd ptr sz

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
    retryEINTR $ orErrno $ c_write fd ptr sz

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

-- | Wrapper around write that makes sure the full bytestring is written,
-- handling short writes from the underlying system call.
writeFull :: Fd -> BS.ByteString -> EIO ()
writeFull fd bs = do
    ret <- write fd bs
    case ret of
        Left e -> pure $ Left e
        Right v
            | (fromIntegral v) == BS.length bs -> pure $ Right ()
            | otherwise -> writeFull fd (BS.drop (fromIntegral v) bs)

writeFullExn :: Fd -> BS.ByteString -> IO ()
writeFullExn fd bs =
    throwIfErrno $ writeFull fd bs

writevBuf :: Fd -> Ptr CIOVec -> CInt -> EIO CSsize
writevBuf fd ptr sz =
    retryEINTR $ orErrno $ c_writev fd ptr sz

writevBufExn :: Fd -> Ptr CIOVec -> CInt -> IO CSsize
writevBufExn fd ptr sz =
    throwIfErrno $ writevBuf fd ptr sz

writevVec :: Fd -> SMV.IOVector CIOVec -> EIO CSsize
writevVec fd vec =
    useIOVecs vec $ \ptr ->
        writevBuf fd ptr (fromIntegral (SMV.length vec))

writevVecExn :: Fd -> SMV.IOVector CIOVec -> IO CSsize
writevVecExn fd vec =
    throwIfErrno $ writevVec fd vec

writev :: Fd -> [BS.ByteString] -> EIO CSsize
writev fd bss =
    unsafeWithByteStrings bss (writevVec fd)

writevExn :: Fd -> [BS.ByteString] -> IO CSsize
writevExn fd bss =
    throwIfErrno $ writev fd bss

-- | Analouge of writeFull, but for writev. Note: This may modify the
-- vector in place (but not the individual buffers it points to).
writevVecFull :: Fd -> SMV.IOVector CIOVec -> EIO CSsize
writevVecFull fd vec =
    go fd vec 0
  where
    go fd vec !written
        | SMV.null vec = pure (Right written)
        | otherwise = do
            res <- writevVec fd vec
            case res of
                Left e -> pure $ Left e
                Right v -> do
                    vec' <- dropBytes (fromIntegral v) vec
                    go fd vec' (written + v)

writevVecFullExn :: Fd -> SMV.IOVector CIOVec -> IO CSsize
writevVecFullExn fd vec =
    throwIfErrno $ writevVecFull fd vec

writevFull :: Fd -> [BS.ByteString] -> EIO CSsize
writevFull fd bss =
    unsafeWithByteStrings bss (writevVecFull fd)

writevFullExn :: Fd -> [BS.ByteString] -> IO CSsize
writevFullExn fd bss =
    throwIfErrno $ writevFull fd bss

remove :: CString -> EIO ()
remove fpath =
    useCStr fpath $ \path ->
        retryEINTR $ orErrno $ () <$ c_remove path

removeExn :: CString -> IO ()
removeExn fpath =
    throwIfErrno $ remove fpath

rmdir :: CString -> EIO ()
rmdir fpath =
    useCStr fpath $ \path ->
        retryEINTR $ orErrno $ () <$ c_rmdir path

rmdirExn :: CString -> IO ()
rmdirExn fpath =
    throwIfErrno $ rmdir fpath

mkdir :: CString -> CMode -> EIO ()
mkdir fpath mode =
    useCStr fpath $ \path ->
        retryEINTR $ orErrno $ () <$ c_mkdir path mode

mkdirExn :: CString -> CMode -> IO ()
mkdirExn fpath mode =
    throwIfErrno $ mkdir fpath mode

newtype OpenFlag = OpenFlag CInt
instance Semigroup OpenFlag where
    (OpenFlag x) <> (OpenFlag y) = OpenFlag (x .|. y)

open :: CString -> OpenFlag -> CMode -> EIO Fd
open fpath (OpenFlag flag) mode =
    useCStr fpath $ \path ->
        retryEINTR $ orErrno $ c_open path flag mode

openExn :: CString -> OpenFlag -> CMode -> IO Fd
openExn path flag mode =
    throwIfErrno $ open path flag mode

openat :: Fd -> CString -> OpenFlag -> CMode -> EIO Fd
openat fd fpath (OpenFlag flag) mode =
    useCStr fpath $ \path ->
        retryEINTR $ orErrno $ c_openat fd path flag mode

openatExn :: Fd -> CString -> OpenFlag -> CMode -> IO Fd
openatExn fd path flag mode =
    throwIfErrno $ openat fd path flag mode

o_APPEND    = OpenFlag c_O_APPEND
o_CLOEXEC   = OpenFlag c_O_CLOEXEC
o_CREAT     = OpenFlag c_O_CREAT
o_DIRECTORY = OpenFlag c_O_DIRECTORY
o_EXCL      = OpenFlag c_O_EXCL
o_NOFOLLOW  = OpenFlag c_O_NOFOLLOW
o_NONBLOCK  = OpenFlag c_O_NONBLOCK
o_NDELAY    = OpenFlag c_O_NDELAY
o_TRUNC     = OpenFlag c_O_TRUNC
o_RDONLY    = OpenFlag c_O_RDONLY
o_WRONLY    = OpenFlag c_O_WRONLY
o_RDWR      = OpenFlag c_O_RDWR

close :: Fd -> EIO ()
close fd =
    orErrno $ void $ c_close fd
    -- We intentionally do not retryEINTR; posix is silent on whether
    -- the file descriptor will be closed after an EINTR return, but on
    -- at least Linux the answer is always yes, so we must not retry
    -- in case the file descriptor has been re-allocated by another thread.

closeExn :: Fd -> IO ()
closeExn fd = throwIfErrno $ close fd
