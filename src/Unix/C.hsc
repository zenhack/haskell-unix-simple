{-# LANGUAGE InterruptibleFFI #-}
module Unix.C
    ( module X
    , c_close
    , c_fdatasync
    , c_fsync
    , c_ftruncate
    , c_mkdir
    , c_open
    , c_openat
    , c_pread
    , c_pwrite
    , c_pwritev
    , c_read
    , c_remove
    , c_rmdir
    , c_write
    , c_writev

    , c_O_APPEND
    , c_O_CLOEXEC
    , c_O_CREAT
    , c_O_DIRECTORY
    , c_O_EXCL
    , c_O_NOFOLLOW
    , c_O_NONBLOCK
    , c_O_NDELAY
    , c_O_TRUNC
    , c_O_RDONLY
    , c_O_WRONLY
    , c_O_RDWR
    ) where

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

import Zhp

import CString (CStr(..))
import Foreign.C.Types    as X
import Foreign.Ptr        as X
import System.Posix.Types as X
import Unix.IOVec (CIOVec)

foreign import ccall interruptible "close" c_close :: Fd -> IO CInt
foreign import ccall interruptible "fdatasync" c_fdatasync :: Fd -> IO CInt
foreign import ccall interruptible "fsync" c_fsync :: Fd -> IO CInt
foreign import ccall interruptible "ftruncate" c_ftruncate :: Fd -> COff -> IO Int
foreign import ccall interruptible "mkdir" c_mkdir :: CStr -> CMode -> IO CInt
foreign import ccall interruptible "openat" c_openat :: Fd -> CStr -> CInt -> CMode -> IO Fd
foreign import ccall interruptible "open" c_open :: CStr -> CInt -> CMode -> IO Fd
foreign import ccall interruptible "pread"  c_pread  :: Fd -> Ptr Word8 -> CSize -> COff -> IO CSsize
foreign import ccall interruptible "pwrite" c_pwrite :: Fd -> Ptr Word8 -> CSize -> COff -> IO CSsize
foreign import ccall interruptible "pwritev" c_pwritev :: Fd -> Ptr CIOVec -> CInt -> COff -> IO CSsize
foreign import ccall interruptible "read"  c_read :: Fd -> Ptr Word8 -> CSize -> IO CSsize
foreign import ccall interruptible "remove"  c_remove :: CStr -> IO CInt
foreign import ccall interruptible "rmdir"  c_rmdir :: CStr -> IO CInt
foreign import ccall interruptible "write" c_write :: Fd -> Ptr Word8 -> CSize -> IO CSsize
foreign import ccall interruptible "writev" c_writev :: Fd -> Ptr CIOVec -> CInt -> IO CSsize

c_O_APPEND :: CInt
c_O_APPEND = #const O_APPEND

c_O_CLOEXEC :: CInt
c_O_CLOEXEC = #const O_CLOEXEC

c_O_CREAT :: CInt
c_O_CREAT = #const O_CREAT

c_O_DIRECTORY :: CInt
c_O_DIRECTORY = #const O_DIRECTORY

c_O_EXCL :: CInt
c_O_EXCL = #const O_EXCL

c_O_NOFOLLOW :: CInt
c_O_NOFOLLOW = #const O_NOFOLLOW

c_O_NONBLOCK :: CInt
c_O_NONBLOCK = #const O_NONBLOCK

c_O_NDELAY :: CInt
c_O_NDELAY = #const O_NDELAY

c_O_TRUNC :: CInt
c_O_TRUNC = #const O_TRUNC

c_O_RDONLY :: CInt
c_O_RDONLY = #const O_RDONLY

c_O_WRONLY :: CInt
c_O_WRONLY = #const O_WRONLY

c_O_RDWR :: CInt
c_O_RDWR = #const O_RDWR
