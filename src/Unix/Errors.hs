module Unix.Errors
    ( retryEintr
    , throwIfErrno
    ) where

import Zhp

import Control.Exception (throwIO)
import Foreign.C.Error

throwIfErrno :: IO (Either Errno a) -> IO a
throwIfErrno io = do
    r <- io
    case r of
        Left e  -> throwIO $ errnoToIOError "" e Nothing Nothing
        Right v -> pure v

retryEintr :: IO (Either Errno a) -> IO (Either Errno a)
retryEintr io = do
    r <- io
    case r of
        Left e | e == eINTR -> retryEintr io
        _                   -> pure r
