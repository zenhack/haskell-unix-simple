module Unix.C
    ( module X
    , CStr(..)
    , c_fdatasync
    , c_fsync
    , c_ftruncate
    , c_open
    , c_openat
    , c_pread
    , c_pwrite
    , c_read
    , c_write

    , c_O_APPEND
    , c_O_CLOEXEC
    , c_O_CREAT
    , c_O_DIRECTORY
    ) where

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

import Zhp

import Foreign.C.Types    as X
import Foreign.Ptr        as X
import System.Posix.Types as X

newtype CStr = CStr (Ptr CChar)

foreign import ccall "fdatasync" c_fdatasync :: Fd -> IO CInt
foreign import ccall "fsync" c_fsync :: Fd -> IO CInt
foreign import ccall "ftruncate" c_ftruncate :: Fd -> COff -> IO Int
foreign import ccall "openat" c_openat :: Fd -> CStr -> CInt -> CMode -> IO Fd
foreign import ccall "open" c_open :: CStr -> CInt -> CMode -> IO Fd
foreign import ccall "pread"  c_pread  :: Fd -> Ptr Word8 -> CSize -> COff -> IO CSsize
foreign import ccall "pwrite" c_pwrite :: Fd -> Ptr Word8 -> CSize -> COff -> IO CSsize
foreign import ccall "read"  c_read  :: Fd -> Ptr Word8 -> CSize -> IO CSsize
foreign import ccall "write" c_write :: Fd -> Ptr Word8 -> CSize -> IO CSsize

c_O_APPEND :: CInt
c_O_APPEND = #const O_APPEND

c_O_CLOEXEC :: CInt
c_O_CLOEXEC = #const O_CLOEXEC

c_O_CREAT :: CInt
c_O_CREAT = #const O_CREAT

c_O_DIRECTORY :: CInt
c_O_DIRECTORY = #const O_DIRECTORY

-- vim: set ft=haskell :
