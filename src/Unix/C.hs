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
    ) where

import Zhp

import Foreign.C.Types    as X
import Foreign.Ptr        as X
import System.Posix.Types as X

newtype CStr = CStr (Ptr CChar)

foreign import ccall "fdatasync" c_fdatasync :: Fd -> IO CInt
foreign import ccall "fsync" c_fsync :: Fd -> IO CInt
foreign import ccall "ftruncate" c_ftruncate :: Fd -> COff -> IO Int
foreign import ccall "openat" c_openat :: Fd -> CStr -> CInt -> CMode -> IO Int
foreign import ccall "open" c_open :: CStr -> CInt -> CMode -> IO Int
foreign import ccall "pread"  c_pread  :: Fd -> Ptr Word8 -> CSize -> COff -> IO CSsize
foreign import ccall "pwrite" c_pwrite :: Fd -> Ptr Word8 -> CSize -> COff -> IO CSsize
foreign import ccall "read"  c_read  :: Fd -> Ptr Word8 -> CSize -> IO CSsize
foreign import ccall "write" c_write :: Fd -> Ptr Word8 -> CSize -> IO CSsize
